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

;;; Commentary:

;;; this package is for making arbitrary links across text files

(defcustom blk-directories
  (list (expand-file-name "~/notes/")
        (file-name-parent-directory (expand-file-name user-init-file)))
  "directories to look for files in")

(defcustom blk-list-directories-recursively
  nil
  "whether to look for files recursively in `blk-directories', if set to `t' may have severe consequences on speed"
  )

(defcustom blk-emacs-patterns
  (list (list :title "titled files" ;; name of the pattern
              :filename-regex ".*\\.org"
              :anchor-regex "^#\\+title: [^:\n]+"
              :title-function 'blk-value-after-space
              :extract-id-function (lambda (matched-text) ()))
        (list :title "elisp functions in config"
              :filename-regex ".*init\\.el"
              :anchor-regex "^\(defun [^\s]+"
              :title-function 'blk-value-after-space
              :extract-id-function 'identity)
        )
  ":title is the title/type of the pattern, :filename-regex is the regex to match files to be grepped :anchor-regex is the regex for matching blocks of text that contain the target value which is then passed to :title-function to be turned into the final desired value to be passed to completing-read and that identifies the target, :link-function is the function that gets the id to be used when creating links to the target, the need for :link-function over :title-function is that an id and a name for the target can be different, as an id can be a random sequence but a name could be a more memorable sequence of characters. if the user wants the id to be the name itself, they may only supply :title-function"
  )

(defcustom blk-grep-patterns
  (list)
  "the pattern table for grep")

(defcustom blk-rg-patterns
  (list (list :title "titled org file or block"
              :filename-regex ".*\\.org"
              :anchor-regex "(:title|:alias|#\\+title:|#\\+alias:|#\\+name:)\\s+[^:\\n]+"
              :title-function 'blk-value-after-space
              :extract-id-function
              (lambda (grep-data)
                (let ((elm (org-element-at-point)))
                  (when elm
                    (let* ((elm-type (org-element-type elm))
                           (id (cl-case elm-type
                                 ('special-block (org-element-property :name elm))
                                 ('keyword (or
                                            ;; for denote
                                            (car (alist-get
                                                  "IDENTIFIER"
                                                  (org-collect-keywords '("identifier"))
                                                  nil nil 'string=))
                                            ;; for org id's (with or without org-roam)
                                            (org-id-get))))))
                      (or id (plist-get grep-data :value)))))))
        (list :title "elisp function"
              :filename-regex ".*\\.el"
              :anchor-regex "^\\(defun\\s+[^\\s]+"
              :title-function 'blk-value-after-space)
        (list :title "org header"
              :filename-regex ".*\\.org"
              :anchor-regex "^\\*+\\s.*"
              :title-function 'blk-value-after-space
              :extract-id-function (lambda (grep-data)
                                     (or (org-id-get) (plist-get grep-result :value))))
        (list :filename-regex ".*\\.org"
              :anchor-regex "^:ID:\\s*"
              :src-id-function 'blk-org-id-value)
        (list :filename-regex ".*\\.org"
              :anchor-regex "\\[\\[[a-z]+:[^\\[\\]]+\\]\\]|\\[\\[[a-z]+:[^\\[\\]]+\\]\\[[^\\[\\]]+\\]\\]"
              :dest-id-function 'blk-org-link-path)
        (list :filename-regex ".*\\.org"
              :anchor-regex "#+identifier:\\s+.*"
              :src-id-function 'blk-value-after-colon)
        (list :title "latex label"
              :filename-regex ".*\\.\\(org\\|tex\\)"
              :anchor-regex "\\\\\\\\label\\\\{[^\\\\{\\\\}]*\\\\}"
              ;; :anchor-regex (shell-quote-argument "\\\\label\\{[^\\{\\}]*\\}")
              :src-id-function 'blk-latex-label-id)
        (list :title "id anchor for org named block"
              :filename-regex ".*\\.org"
              :anchor-regex "#\\+name:\\s+.*|:name\\s+[^:]*"
              :src-id-function 'blk-value-after-space-before-colon
              :transclusion-function
              (lambda (grep-data)
                (let ((elm (org-element-at-point)))
                  (when elm
                    (let* ((elm-type (org-element-type elm)))
                      (cl-case elm-type
                        ('special-block
                         (list :src-content (buffer-substring (org-element-property :begin elm)
                                                              (org-element-property :end elm))
                               :src-buf (current-buffer)
                               :src-beg (org-element-property :begin elm)
                               :src-end (org-element-property :end elm))))
                      ;; ('keyword (buffer-string))
                      )))))
        )
  "the pattern table for ripgrep")

(defcustom blk-insert-patterns
  (list (list :filename-regex ".*\\.org"
              :id-format "[[blk:%i][%t]]")
        (list :filename-regex ".*\\.tex"
              :id-format "\\ref{blk:%i}")
        (list :filename-regex ".*\\.el"
              :id-format "blk:%i")
        )
  "the patterns for inserting links, :filename-regex is for matching with filenames, and id-format is for inserting the link into a buffer, %i will be replaced by the target id and %t by the target's title, if existent")

(defun blk-value-after-space (str)
  (string-trim (string-join (cdr (split-string str " ")) " ")))

(defun blk-org-link-path (org-link-text)
   (car (split-string (cadr (split-string org-link-text ":")) "]")))

(defun blk-org-id-value (org-id-text)
  (caddr (split-string org-id-text ":")))

(defun blk-value-after-colon (text)
  (cadr (split-string text ":")))

(defun blk-latex-label-id (text)
  (car (split-string (cadr (split-string text "{")) "}")))

(defun blk-value-after-space-before-colon (str)
  (string-trim (car (split-string (blk-value-after-space str) ":"))))

(defcustom blk-grepper
  (choose-grepper)
  "the program to use for grepping files, could be a function that takes as arguments the patterns and files, or a string representing a shell command to be formatted with the regex to grep for and the file list")

(defcustom blk-patterns
  (cl-case (choose-grepper)
    (blk-grepper-rg blk-rg-patterns)
    (blk-grepper-grep blk-grep-patterns)
    (blk-grepper-emacs blk-emacs-patterns))
  "the list of patterns to invoke the grepper with")

(defconst
  blk-grepper-grep
  '(:command "grep -e '%s' '%s' --no-heading --line-number --ignore-case --byte-offset --only-matching"
             :separator ":"
             :regex-or "|"))

(defconst
  blk-grepper-rg
  (list :command "rg --field-match-separator '\t' --regexp \"%r\" %f --no-heading --line-number --ignore-case --byte-offset --only-matching --with-filename"
        :delimiter "\t"))

(defun choose-grepper ()
  "choose a grepper depending on whether it can be found in `exec-path', fall back to the `blk-grepper-emacs' function"
  (cond
      ((locate-file "rg" exec-path) blk-grepper-rg)
      ((locate-file "grep" exec-path) blk-grepper-grep)
      (_ blk-grepper-emacs)))

(defmacro blk-with-file-as-current-buffer (file &rest body)
  (let ((present-buffer (gensym))
        (result (gensym)))
    `(let ((,present-buffer (find-buffer-visiting ,file)))
       (save-excursion
         (with-current-buffer (find-file-noselect ,file)
           (setq ,result (progn ,@body))
           (when (not ,present-buffer)
             (kill-buffer (current-buffer)))
           ,result)))))

(defun blk-grepper-emacs (pattern-table files)
  (let ((results))
    (dolist (pattern pattern-table)
      (let ((matched-buffers
             (blk-str-list-matches (plist-get pattern :filename-regex)
                                   (mapcar 'buffer-file-name (blk-list-buffers))))
            (matched-files
             (blk-str-list-matches (plist-get pattern :filename-regex)
                                   (blk-list-files))))
        (dolist (filename (cl-union matched-buffers matched-files))
          (blk-with-file-as-current-buffer
           filename
           (let ((matches (blk-string-search-regex (plist-get pattern :anchor-regex)
                                                   (substring-no-properties (buffer-string)))))
             (dolist (match matches)
               (push (list :position (cdr match)
                           :filepath buffer-file-name
                           :value (funcall (plist-get pattern :title-function) (car match))
                           :matched-pattern pattern)
                     results)))))))
    results))

(defun blk-string-search-regex (regex str)
  "returns matches of `regex' found in the string `str', a list with conses of the form (match . position) is returned"
  (let ((pos 0)
        (matches))
    (cl-loop for match-pos = (string-match regex str pos)
             while match-pos do
             (push (cons (match-string 0 str) match-pos) matches)
             (setq pos (1+ match-pos)))
    matches))

(defun blk-list-files ()
  "list the directories `blk-directories' to use for grepping links/references"
  (let ((files))
    (dolist (dir blk-directories)
      (setq files (append files (if blk-list-directories-recursively
                                    (mapcar (lambda (filename) (concat dir "/" filename))
                                            (directory-files-recursively dir ""))
                                  (mapcar (lambda (filename)
                                            (concat dir "/" filename))
                                          (directory-files dir))))))
    files))

(defun blk-list-buffers ()
  (buffer-list))

(defun blk-list-entries ()
  (let* ((grep-results
          (mapcar
           (lambda (grep-result)
             (plist-put grep-result
                        :value
                        (funcall (plist-get (plist-get grep-result :matched-pattern) :title-function)
                                 (plist-get grep-result :value))))
           (blk-grep
            blk-grepper
            (cl-remove-if-not (lambda (pattern) (plist-get pattern :title-function)) blk-patterns)
            (blk-list-files))))
         (entries (mapcar (lambda (grep-result)
                            (propertize (plist-get grep-result :value) 'grep-data grep-result))
                          grep-results)))
    entries))

(defun blk-str-list-matches (str-list regex)
  "get the strings from `str-list' matching the regex `regex'"
  (cl-remove-if-not
   (lambda (str)
     (string-match regex str))
   str-list))

(defun blk-run-grep-cmd (cmd patterns files)
  (let ((matches))
    (dolist (pattern patterns)
      (let* ((matching-files (blk-str-list-matches files (plist-get pattern :filename-regex)))
             (files-str (string-join (mapcar 'shell-quote-argument matching-files) " "))
             (full-cmd (format-spec (plist-get cmd :command)
                                    `((?f . ,files-str)
                                      (?r . ,(plist-get pattern :anchor-regex)))))
             (out (shell-command-to-string full-cmd))
             (sep (plist-get cmd :delimiter)))
        (dolist (line (split-string out "\n"))
          (when (not (string-empty-p line))
            (let* ((line-entries (split-string line sep))
                   (filepath (car line-entries))
                   (line-number (string-to-number (cadr line-entries)))
                   (position (string-to-number (caddr line-entries)))
                   (match-text (string-join (cdddr line-entries) sep)))
              (push (list :value match-text
                          :position (1+ position) ;; grep starts at position 0, while emacs doesnt
                          :line-number line-number
                          :matched-pattern pattern
                          :filepath filepath)
                    matches))))))
    matches))

;;;###autoload
(defun blk-find (text)
  "find entries defined by patterns in `blk-patterns' using the grepper `blk-grepper', when found, visit it"
  (interactive
   (list (let* ((minibuffer-allow-text-properties t)
                (entries (blk-list-entries))
                (completion-extra-properties
                 '(:annotation-function
                   (lambda (key)
                     (format "\t%s" (plist-get (plist-get (get-text-property 0 'grep-data key)
                                                          :matched-pattern)
                                               :title))))))
           (completing-read "entry " entries))))
  (if (get-text-property 0 'grep-data text)
      (let* ((grep-data (get-text-property 0 'grep-data text))
             (filepath (plist-get grep-data :filepath))
             (position (plist-get grep-data :position)))
        (find-file filepath)
        (goto-char position))
    (message "%s not found" text)))

(defun blk-insert (text)
  "insert a link to entries defined by patterns in `blk-patterns' using the grepper `blk-grepper', when found, visit it, the pattern of the link is defined by entries in `blk-insert-patterns'"
  (interactive
   (list (let ((minibuffer-allow-text-properties t))
           (completing-read "entry " (blk-list-entries)))))
  (if (get-text-property 0 'grep-data text)
      (let* ((grep-data (get-text-property 0 'grep-data text))
             (grep-pattern (plist-get grep-data :matched-pattern))
             (id-pattern (cl-find-if (lambda (id-pattern)
                                       (string-match (plist-get id-pattern :filename-regex)
                                                     buffer-file-name))
                                     blk-insert-patterns))
             (extract-id-func (plist-get grep-pattern :extract-id-function)))
        (if extract-id-func
            (let ((id (blk-with-file-as-current-buffer
                       (plist-get grep-data :filepath)
                       (goto-char (plist-get grep-data :position))
                       (funcall extract-id-func grep-data))))
              (if id
                  (progn (insert (format-spec (plist-get id-pattern :id-format)
                                          `((?t . ,(substring-no-properties text))
                                            (?i . ,id)))))
                (message "match has no id")))
          (message "pattern has no extract-id-function")))
    (message "%s not found" text)))

(defun blk-grep (grepper patterns files)
  "run the grepper on the given patterns and files, `greper' could be a function that takes in the pattern tables and files as arguments, see `blk-emacs-grep', or a property list describing a shell command, see `blk-grepper-grep', `blk-grepper-grep'"
  (when (functionp grepper)
    (funcall blk-emacs-grep patterns files))
  (when (listp grepper)
    (blk-run-grep-cmd grepper patterns files)))

(defun blk-find-links-to-id (id)
  "find links that point to `id'"
  (let* ((id-patterns
          (cl-remove-if
           (lambda (pattern)
             (plist-get pattern :title-function))
           blk-patterns))
         (grep-results
          (cl-remove-if-not
           (lambda (entry)
             (equal (plist-get entry :target-id) id))
           (mapcar
            (lambda (grep-result)
              (plist-put grep-result
                         :target-id
                         (funcall (plist-get (plist-get grep-result :matched-pattern) :id-function)
                                  (plist-get grep-result :value))))
            (blk-grep
             blk-grepper
             id-patterns
             (blk-list-files))))))
    grep-results))

(defun blk-find-by-id (id)
  "find by `id', if a pattern and entry are matched"
  (let* ((id-patterns
          (cl-remove-if-not
           (lambda (pattern)
             (plist-get pattern :src-id-function))
           blk-patterns))
         (grep-results
          (cl-remove-if-not
           (lambda (entry)
             (equal (plist-get entry :id) id))
           (mapcar
            (lambda (grep-result)
              (plist-put grep-result
                         :id
                         (funcall (plist-get (plist-get grep-result :matched-pattern) :src-id-function)
                                  (plist-get grep-result :value))))
            (blk-grep
             blk-grepper
             id-patterns
             (blk-list-files))))))
    grep-results))

(require 'blk-org)

(provide 'blk)
;; blk.el ends here