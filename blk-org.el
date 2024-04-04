;; Copyright (C) 2024  Mahmood Sheikh

;; Author: mahmood sheikh <mahmod.m2015@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

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

;;;###autoload
(defun blk-configure-org-link ()
  "create the blk org link type"
  (org-link-set-parameters "blk"
                           :follow #'org-blk-open
                           :export #'org-blk-export))

;;;###autoload
(defun blk-configure-org-transclusion ()
  "auto configure org-transclusion integration with blk"
  (add-to-list 'org-transclusion-add-functions 'org-transclusion-add-blk))

(defun org-blk-open (link _)
  "open the file containing a block with the id `link'"
  (let ((result (car (blk-find-by-id link))))
    (find-file (plist-get result :filepath))
    (goto-char (plist-get result :position))))

(defun org-blk-export (link desc format)
  "return the file containing a block with the id `link' for org exporting purposes"
  (if (org-blk-find-anchor link)
      (let* ((linked-file (car (blk-find-by-id link)))
             (desc (or desc link))
             (linked-file-no-ext (file-name-sans-extension linked-file)))
        (cond
         ((eq format 'html) (format "<a href=\"%s.html\">%s</a>" linked-file-no-ext desc))
         ((eq format 'md) (format "[%s](%s.md)" desc linked-file-no-ext))
         (t link)))
    link))

(defun org-transclusion-add-blk (link plist)
  "return a list for blk link object and plist. return nil if not found."
  (when (string= "blk" (org-element-property :type link))
    (let* ((id (org-element-property :path link))
           (result (ignore-errors (car (blk-find-by-id id))))
           (payload '(:tc-type "org-link")))
      (if result
          (progn
            ;; for now we cant work without keeping the file opened in an emacs buffer (this is how org-transclusion handles things)
            (find-file-noselect (plist-get result :filepath))
            (with-current-buffer
             (plist-get result :filepath)
             (goto-char (plist-get result :position))
             (append payload
                     (funcall (plist-get (plist-get result :matched-pattern) :transclusion-function) result))))
        (progn (message (format "no transclusion done for this blk. ensure it works at point %d, line %d"
                            (point) (org-current-line)))
           nil)))))

(defalias
  'blk-open-at-point
  'org-open-at-point-global
  "an alias to `org-open-at-point-global', to allow links to be openable in any major mode, defined for convenience.")

(provide 'blk-org)
;;; blk-org.el ends here