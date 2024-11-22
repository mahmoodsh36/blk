;;; blk-org.el --- Add blk-type Org links and transclusion   -*- lexical-binding: t; -*-

;; Author: Mahmood Sheikh <mahmod.m2015@gmail.com>
;; Keywords: lisp
;; Version: 0.0.2

;; Copyright (C) 2024  Mahmood Sheikh and Bob Weiner

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package is for making arbitrary links across text files.

;;; Code:
;;;###autoload
(defun blk-configure-org-link ()
  "Create the blk org link type."
  (org-link-set-parameters "blk"
                           :follow #'blk-org-open
                           :export #'blk-org-export))

;;;###autoload
(defun blk-configure-org-transclusion ()
  "Auto configure `org-transclusion' integration with blk."
  (add-to-list 'org-transclusion-add-functions 'blk-org-transclusion))

(defun blk-org-open (link _)
  "Open the file containing a block with the LINK id."
  (when (not (blk-open-by-id link))
    (message "id %s not found" link)))

(defun blk-org-export (link desc format)
  "Return the LINK with DESC converted into html or markdown FORMAT.
If LINK is not found, just return it as is."
  (if (blk-find-by-id link)
      (let* ((linked-file (plist-get (car (blk-find-by-id link)) :filepath))
             (desc (or desc link))
             (linked-file-no-ext (file-name-sans-extension (org-export-file-uri linked-file))))
        (cond
         ((eq format 'html) (format "<a href=\"%s.html#%s\">%s</a>" linked-file-no-ext link desc))
         ((eq format 'md) (format "[%s](%s.md)" desc linked-file-no-ext))
         ((eq format 'latex) (format "\\hyperref[%s]{%s}" link (or desc link)))
         (t link)))
    link))

(defun blk-org-transclusion (link plist)
  "Return an `org-transclusion' list for blk LINK and PLIST.
Return nil if not found."
  (when (string= "blk" (org-element-property :type link))
    (let* ((id (org-element-property :path link))
           (result (ignore-errors (car (blk-find-by-id id))))
           (payload '(:tc-type "org-link")))
      (if result
          (progn
            ;; For now we can't work without keeping the file opened in
	    ;; an emacs buffer (this is how org-transclusion handles
	    ;; things)
            (find-file-noselect (plist-get result :filepath))
            (with-current-buffer
             (find-buffer-visiting (plist-get result :filepath))
             (goto-char (plist-get result :position))
             (append payload
                     (funcall (plist-get (plist-get result :matched-pattern) :transclusion-function)
                              result))))
        (progn (message (format "No transclusion done for this blk.  Ensure it works at point %d, line %d."
                            (point) (org-current-line)))
           nil)))))

(defun blk-org-id-at-point (grep-data)
  "Get the id to the org element at point.
If no id can be found, interactively select one from the results of
calling grep using GREP-DATA."
  (let* ((elm (org-element-at-point))
         ;; block-name isnt necessarily gonna be defined, may be nil when its not
         ;; a block we're at, or if the block we're at doesnt define a :name
         (block-name
          (or (org-element-property :name (org-element-at-point)) ;; for #+name: keyword before block
              (alist-get ;; for :name in #+begin_something :name...
               :name
               (org-babel-parse-header-arguments
                (org-element-property :parameters elm)
                t)))))
    (when elm
      (let* ((elm-type (org-element-type elm))
             (id (cond
                  ;; if we are at a block and it has a name, return that, otherwise return the link to the file
                  ((and (member elm-type '(special-block latex-environment src-block)) block-name)
                   block-name)
                  ;; for links to files, through org-id or denote #+identifier
                  ((or (eq elm-type 'keyword)
                       (and (eq elm-type 'special-block)
                            (not block-name)))
                   (or
                    ;; for denote
                    (car (alist-get
                          "IDENTIFIER"
                          (org-collect-keywords '("identifier"))
                          nil nil 'string=))
                    ;; for an org id (with or without org-roam)
                    (org-id-get)))
                  ;; if we are at a header, return its id (might return nil or id of file if header doesnt have id)
                  ((eq elm-type 'headline) (org-id-get)))))
        id))))

(defun blk-org-transclusion-at-point (grep-data)
  "Function that return a DWIM org-transclusion plist.
the plist returned represents an org-transclusion object which is then passed to
org-transclusion to be handled for transclusion in an org buffer."
  (let ((elm (org-element-at-point)))
    (when elm
      (let* ((elm-type (org-element-type elm)))
        (cond
         ;; handler for custom/src org-blocks
         ((or (eq elm-type 'special-block) (eq elm-type 'src-block))
          (list :src-content (buffer-substring (org-element-property :begin elm)
                                               (org-element-property :end elm))
                :src-buf (current-buffer)
                :src-beg (org-element-property :begin elm)
                :src-end (org-element-property :end elm)))
         ;; handler for latex blocks identified by #+name
         ((eq elm-type 'latex-environment)
          (progn
            (forward-line)
            (blk-tex-transclusion-env-at-point grep-data)))
         ((and (equal elm-type 'keyword)
               (equal (org-element-property :key elm) "IDENTIFIER"))
          ;; skip over the file keywords
          (save-excursion
            (let ((no-text))
              (while (and (equal (org-element-type (org-element-at-point)) 'keyword)
                          (not no-text))
                ;; check if we are at the last line
                (if (eq (line-number-at-pos)
                        (line-number-at-pos (point-max)))
                    (setq no-text t)
                  (forward-line)))
              ;; file contains no text except the keywords, dont transclude
              (when (not no-text)
                (list :src-content (buffer-substring (point)
                                                     (point-max))
                      :src-buf (current-buffer)
                      :src-beg (point)
                      :src-end (point-max)))))))))))

(defun blk-org-named-target-value (str)
  "For an STR equal to <<<my-target>>>, returns my-target."
  (substring str 3 -3))

(defalias
  'blk-open-at-point
  'org-open-at-point-global
  "A convenience alias to `org-open-at-point-global', works in any major mode.")

;; Required for blk-open-at-point to work
(blk-configure-org-link)

(provide 'blk-org)
;;; blk-org.el ends here
