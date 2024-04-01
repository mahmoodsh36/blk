;;;###autoload
(defun blk-configure-org-link ()
  (org-link-set-parameters "blk"
                           :follow #'org-blk-open
                           :export #'org-blk-export)
  )

;;;###autoload
(defun blk-configure-org-transclude ()
  (add-to-list 'org-transclusion-add-functions 'org-transclusion-add-blk))

(defun org-blk-open (link _)
  "open the file containing a block with the name `link'"
  (let* ((result (car (blk-find-by-id link))))
    (find-file (plist-get result :filepath))
    (goto-char (plist-get result :position))))

(defun org-blk-export (link desc format)
  "return the file containing a block with the name `link' for org exporting purposes"
  (if (org-blk-find-anchor link)
      (let* ((linked-file (car (blk-find-by-id link)))
             (desc (or desc link))
             (linked-file-no-ext (file-name-sans-extension (file-name-base linked-file))))
        (cond
         ((eq format 'html) (format "<a href=\"%s.html\">%s</a>" linked-file-no-ext desc))
         ((eq format 'md) (format "[%s](%s.md)" desc linked-file-no-ext))
         (t link)))
    link))

(defun org-blk-marker (link _)
  "open the file containing a block with the name `link'"
  (let* ((position (org-blk-find-anchor link))
         (filepath (car position))
         (line (cadr position)))
    ;; for now we cant work with the marker returned unless we have the file open in a buffer4 after the function returns (unfortunately this is how org-transclusion handles things)
    (find-file-noselect filepath)
    (with-file-as-current-buffer filepath
      (goto-line line)
      (point-marker))))

(defun org-transclusion-add-blk (link plist)
  "Return a list for Org-ID LINK object and PLIST.
Return nil if not found."
  (when (string= "blk" (org-element-property :type link))
    (let* ((path (org-element-property :path link))
           (marker (ignore-errors (org-blk-marker path t)))
           (payload '(:tc-type "org-link")))
      (if marker
          (append payload (org-transclusion-content-org-marker marker plist))
        (message
         (format "no transclusion done for this blk. ensure it works at point %d, line %d"
                 (point) (org-current-line)))
        nil))))

(defalias
  'blk-open-at-point
  'org-open-at-point-global
  "an alias to `org-open-at-point-global', to allow links to be openable in any major mode, defined for convenience.")

(provide 'blk-org)