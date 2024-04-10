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
  (let ((result (car (blk-find-by-id link))))
    (if result
        (progn
          (find-file (plist-get result :filepath))
          (goto-char (plist-get result :position)))
      (message "id %s not found" link))))

(defun blk-org-export (link desc format)
  "Return the LINK with DESC converted into html or markdown FORMAT.
If LINK is not found, just return it as is."
  (if (blk-find-by-id link)
      (let* ((linked-file (plist-get (car (blk-find-by-id link)) :filepath))
             (desc (or desc link))
             (linked-file-no-ext (file-name-sans-extension linked-file)))
        (cond
         ((eq format 'html) (format "<a href=\"%s.html\">%s</a>" linked-file-no-ext desc))
         ((eq format 'md) (format "[%s](%s.md)" desc linked-file-no-ext))
         ((eq format 'latex) (format "\\href{%s.tex}{%s}" linked-file-no-ext desc))
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
                     (funcall (plist-get (plist-get result :matched-pattern) :transclusion-function) result))))
        (progn (message (format "No transclusion done for this blk.  Ensure it works at point %d, line %d."
                            (point) (org-current-line)))
           nil)))))

(defalias
  'blk-open-at-point
  'org-open-at-point-global
  "A convenience alias to `org-open-at-point-global', works in any major mode.")

;; Required for blk-open-at-point to work
(blk-configure-org-link)

(provide 'blk-org)
;;; blk-org.el ends here
