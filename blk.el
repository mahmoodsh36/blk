;;; blk.el --- Rapidly create and follow links across text files  -*- lexical-binding: t; -*-

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
(require 'subr-x)
(require 'cl-lib)

(defcustom blk-directories (list (expand-file-name "~/notes/")
				 (file-name-parent-directory (expand-file-name user-init-file)))
  "Blk directories within which to find files to insert and search for links.")

(defcustom blk-list-directories-recursively nil
  "Non-nil means look for files recursively in `blk-directories'.
Default is nil; changing it may have severe consequences on speed.")

(defcustom blk-emacs-patterns
  (list
   (list :title "titled org file or block"
         :filename-regex ".*\\.org"
         :anchor-regex "\\(:title\\|:alias\\|#\\+title:\\|#\\+alias:\\|#\\+name:\\)\s+[^:]+"
         :title-function 'blk-value-after-space
         :extract-id-function 'blk-org-id-at-point)
   (list :title "elisp function"
         :filename-regex ".*\\.el"
         :anchor-regex "^(defun\s+[^\s]+"
         :title-function 'blk-value-after-space)
   (list :title "org header"
         :filename-regex ".*\\.org"
         :anchor-regex "^\\*+ .*"
         :title-function 'blk-value-after-space
         :extract-id-function 'blk-org-id-at-point)
   (list :filename-regex ".*\\.org"
         :anchor-regex "^:ID:\\s*.*"
         :src-id-function 'blk-org-id-value)
   (list :filename-regex ".*\\.org"
         :anchor-regex org-link-any-re
         :dest-id-function 'blk-org-link-path)
   (list :filename-regex ".*\\.org"
         :anchor-regex "#\\+identifier:\s+.*"
         :src-id-function 'blk-value-after-colon)
   (list :title "latex label"
         :filename-regex ".*\\.\\(org\\|tex\\)"
         :anchor-regex "\\\\label{[^\\{\\}]*}"
         :src-id-function 'blk-latex-label-id)
   (list :title "id anchor for org named block"
         :filename-regex ".*\\.org"
         :anchor-regex "#\\+name:\\s+.*|:name\\s+[^:]*"
         :src-id-function 'blk-value-after-space-before-colon
         :transclusion-function 'blk-org-transclusion-at-point))
  "The pattern table for the elisp grepper; see documentation for `blk-patterns'.")

(defcustom blk-rg-patterns
  (list
   (list :title "titled org file or block"
         :filename-regex ".*\\.org"
         :anchor-regex "(:title|:alias|#\\+title:|#\\+alias:|#\\+name:)\\s+[^:]+"
         :title-function 'blk-value-after-space
         :extract-id-function #'blk-org-id-at-point)
   (list :title "elisp function"
         :filename-regex ".*\\.el"
         :anchor-regex "^\\(defun\\s+\\S+"
         :title-function 'blk-value-after-space)
   (list :title "org header"
         :filename-regex ".*\\.org"
         :anchor-regex "^\\*+\\s.*"
         :title-function 'blk-value-after-space
         :extract-id-function 'blk-org-id-at-point)
   (list :filename-regex ".*\\.org"
         :anchor-regex "^:ID:\\s*.*"
         :src-id-function 'blk-org-id-value)
   (list :filename-regex ".*\\.org"
         :anchor-regex "\\[\\[[a-z]+:[^\\[\\]]+\\]\\]|\\[\\[[a-z]+:[^\\[\\]]+\\]\\[[^\\[\\]]+\\]\\]"
         :dest-id-function 'blk-org-link-path)
   (list :filename-regex ".*\\.org"
         :anchor-regex "#\\+identifier:\\s+.*"
         :src-id-function 'blk-value-after-colon)
   (list :title "latex label"
         :filename-regex ".*\\.\\(org\\|tex\\)"
         :anchor-regex "\\\\\\\\label\\\\{[^\\\\{\\\\}]*\\\\}"
         :src-id-function 'blk-latex-label-id)
   (list :title "id anchor for org named block"
         :filename-regex ".*\\.org"
         :anchor-regex "#\\+name:\\s+.*|:name\\s+[^:]*"
         :src-id-function 'blk-value-after-space-before-colon
         :transclusion-function 'blk-org-transclusion-at-point))
  "The pattern table for ripgrep; see documentation for `blk-patterns'.")

;; grep -E plays well with rg regex's so as far as i can tell no extra work is needed
(defcustom blk-grep-patterns
  blk-rg-patterns
  "the pattern table for grep")

;; Support insertion into editable non-file buffers too.
(defcustom blk-insert-patterns
  (list (list :mode-list '(auctex-mode latex-mode tex-mode)
              :id-format "\\ref{blk:%i}")
	;; By default, match to any other major-mode
	(list :mode-list nil
              :id-format "[[blk:%o]]"))
  "The patterns for inserting links.
:mode-list is for matching buffer major modes.
id-format is for inserting the link into a buffer.
%i will be replaced by the target id.
%o will be use Org link format to include both the id and title, when given.
%t will be replaced by the target title, when given.")

(defun blk-org-id-at-point (grep-data)
  "Get the id to the org element at point.
If no id can be found, interactively select one from the results of
calling grep using GREP-DATA."
  (let ((elm (org-element-at-point)))
    (when elm
      (let* ((elm-type (org-element-type elm))
             (id (cond
                  ;; if we are at a block and it has a name, return that, otherwise return the link to the file
                  ((and (eq elm-type 'special-block)
                        (org-element-property :name elm))
                   (org-element-property :name elm))
                  ;; for links to files, through org-id or denote #+identifier
                  ((or (eq elm-type 'keyword)
                       (and (eq elm-type 'special-block)
                            (not (org-element-property :name elm))))
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
        (or id (plist-get grep-data :value))))))

(defun blk-org-transclusion-at-point (grep-data)
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
                 :src-end (org-element-property :end elm))))))))

(defun blk-value-after-space (str)
  (string-trim (string-join (cdr (split-string str " ")) " ")))

(defun blk-org-link-path (org-link-text)
  (string-trim (car (split-string (cadr (split-string org-link-text ":")) "]"))))

(defun blk-org-id-value (org-id-text)
  (string-trim (caddr (split-string org-id-text ":"))))

(defun blk-value-after-colon (text)
  (string-trim (cadr (split-string text ":"))))

(defun blk-latex-label-id (text)
  (string-trim (car (split-string (cadr (split-string text "{")) "}"))))

(defun blk-value-after-space-before-colon (str)
  (string-trim (car (split-string (blk-value-after-space str) ":"))))

(defconst
  blk-grepper-grep
  '(:command "grep -E -e \"%r\" %f --line-number --ignore-case --byte-offset --only-matching"
             :delimiter ":"))

(defconst
  blk-grepper-rg
  (list :command "rg --field-match-separator '\t' --regexp \"%r\" %f --no-heading --line-number --ignore-case --byte-offset --only-matching --with-filename"
        :delimiter "\t"))

(defun blk-choose-grepper ()
  "Choose a blk grepper based on a search of `exec-path'.
If none are found, default to the `blk-grepper-emacs' function."
  (cond
      ((locate-file "rg" exec-path) blk-grepper-rg)
      ((locate-file "grep" exec-path) blk-grepper-grep)
      (_ blk-grepper-emacs)))

(defcustom blk-grepper
  (blk-choose-grepper)
  "The program for blk to use for grepping files.
Could be a function that takes as arguments the patterns and files, or
a string representing a shell command to be formatted with the regex
to grep for, together with the file list.")

(defcustom blk-patterns
  (pcase (blk-choose-grepper)
    (blk-grepper-rg blk-rg-patterns)
    (blk-grepper-grep blk-grep-patterns)
    (blk-grepper-emacs blk-emacs-patterns))
  "The list of patterns to use with the blk grepper.
Each entry should be a plist representing the data of a pattern:
  :title is the title/type of the pattern (used for completing-read)
  :filename-regex is the regex to match files to be grepped; this
    should always be an emacs regex because matching files is done in
    Elisp.
  :anchor-regex is the regex for matching blocks of text that contain
    the target value which is then passed to :title-function to be
    turned into the final desired value to be passed to completing-read
    and that identifies the target
  :link-function is the function that gets the id to be used when
    creating links to the target; the need for :link-function over
    :title-function is that an id and a name for the target can be
    different, as an id can be a random sequence but a name could be
    a more memorable sequence of characters
  :title-function is used alone to make the id be the name itself.")

(defmacro blk-with-file-as-current-buffer (file &rest body)
  "Macro that reads FILE into the current buffer and executes BODY."
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
  "Function that reads patterns from PATTERN-TABLE and greps for them in FILES."
  (let ((results))
    (let ((all-files (cl-union files
                           (remove nil (mapcar 'buffer-file-name (blk-list-buffers))))))
      (dolist (filepath files)
        (let ((matched-patterns (cl-remove-if-not (lambda (pattern)
                                                    (and (plist-get pattern :title-function)
                                                        (string-match (plist-get pattern :filename-regex)
                                                                      filepath)))
                                                  pattern-table))
              (buf (find-buffer-visiting filepath)))
          ;; if file isnt already opened in some buffer, we open it ourselves, we dont use `find-file-noselect' as using `insert-file-contents' makes the code run alot faster
          (when matched-patterns
            (when (not buf)
              (setq buf (get-buffer-create " blk"))
              (with-current-buffer buf
                (delete-region (point-min) (point-max))
                (insert-file-contents filepath)))
            (with-current-buffer buf
              (dolist (pattern matched-patterns)
                (let ((matches (blk-string-search-regex (plist-get pattern :anchor-regex)
                                                        (substring-no-properties (buffer-string)))))
                  (dolist (match matches)
                    (push (list :position (1+ (cdr match))
                                :filepath filepath
                                :value (car match)
                                :matched-pattern pattern)
                          results)))))))))
    results))

(defun blk-string-search-regex (regex str)
  "Return matches of REGEX found in STR as a list of conses of the form (match . position)."
  (let ((pos 0)
        (matches))
    (cl-loop for match-pos = (string-match regex str pos)
             while match-pos do
             (push (cons (match-string 0 str) match-pos) matches)
             (setq pos (1+ match-pos)))
    matches))

(defun blk-list-files ()
  "Return a list of files in `blk-directories' for grepping links/references.
Recurse subdirectories if `blk-list-directories-recursively' is non-nil."
  (let ((files))
    (dolist (dir blk-directories)
      (setq files (append files (if blk-list-directories-recursively
                                    (mapcar (lambda (filename) (expand-file-name filename dir))
                                            (directory-files-recursively dir ""))
                                  (mapcar (lambda (filename)
					    (expand-file-name filename dir))
                                          (directory-files dir))))))
    files))

(defun blk-list-buffers ()
  "List buffers for searching when `blk-grepper-emacs' is used.
This calls the function `buffer-list' to allow for easy advising of
the function if desired."
  (buffer-list))

(defun blk-list-entries ()
  "List all the pattern matches found in the blk files."
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

(defun blk-str-list-matches (regex str-list)
  "Return a list of the strings matching REGEX in STR-LIST."
  (cl-remove-if-not
   (lambda (str)
     (string-match regex str))
   str-list))

(defun blk-run-grep-cmd (cmd patterns files)
  "Run a grep-like CMD matching any PATTERNS across a list of FILES.
Return a list of lists of key-value pairs of the form:
  '(:value <value>
    :position <buffer-position>
    :line-number <line-number>
    :matched-pattern <grep-pattern>
    :filepath <filepath>)."
  (let (files-str
        full-cmd
	matches
	matching-files
        out
	sep
        line-entries
        filepath
        line-number
        position
        match-text)
    (dolist (pattern patterns)
      (setq matching-files (blk-str-list-matches (plist-get pattern :filename-regex) files))
      (when matching-files
	(setq files-str (string-join (mapcar 'shell-quote-argument matching-files) " ")
	      full-cmd (format-spec (plist-get cmd :command)
                                    `((?f . ,files-str)
                                      (?r . ,(plist-get pattern :anchor-regex))))
              out (shell-command-to-string full-cmd)
	      sep (plist-get cmd :delimiter))
	(dolist (line (split-string out "\n"))
          (when (not (string-empty-p line))
            (setq line-entries (split-string line sep)
		  filepath (car line-entries)
                  line-number (string-to-number (or (cadr line-entries) "1"))
		  position (string-to-number (or (caddr line-entries) "0"))
		  match-text (if (cdddr line-entries) (string-join (cdddr line-entries) sep) ""))
	    (push (list :value match-text
			:position (1+ position) ;; grep starts at position 0, while emacs doesnt
			:line-number line-number
			:matched-pattern pattern
			:filepath filepath)
		  matches)))))
    matches))

;;;###autoload
(defun blk-find (text)
  "Find entries defined by patterns in `blk-patterns' using the grepper `blk-grepper'.
Select one and visit it."
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
  "Insert a link at point to an entry defined by the patterns in `blk-patterns'.
Use the grepper given by `blk-grepper'.  The link format is defined by
entries in `blk-insert-patterns'."
  (interactive
   (progn (barf-if-buffer-read-only)
	  (list (let ((minibuffer-allow-text-properties t))
		  (completing-read "entry " (blk-list-entries))))))
  (barf-if-buffer-read-only)
  (if (get-text-property 0 'grep-data text)
      (let* ((grep-data (get-text-property 0 'grep-data text))
             (grep-pattern (plist-get grep-data :matched-pattern))
             (id-pattern (or (cl-find-if (lambda (id-pattern)
					   (apply #'derived-mode-p (plist-get id-pattern :mode-list)))
					 blk-insert-patterns)
			     (cl-find-if (lambda (id-pattern)
					   ;; Use default pattern, ;; mode-list = nil,
					   ;; meaning match to any other mode.
					   (null (plist-get id-pattern :mode-list)))
					 blk-insert-patterns)))
             (extract-id-func (plist-get grep-pattern :extract-id-function))
	     (text-no-properties (substring-no-properties text)))
        (if extract-id-func
            (let ((id (blk-with-file-as-current-buffer
                       (plist-get grep-data :filepath)
                       (goto-char (plist-get grep-data :position))
                       (funcall extract-id-func grep-data))))
              (if id
                  (progn (if (plist-get id-pattern :id-format)
			     (insert (format-spec (plist-get id-pattern :id-format)
						  `((?i . ,id)
						    ;; Org [[link]] or [[link][title]] format
						    (?o . ,(if (equal id text-no-properties)
							       id
							     (concat id "][" text-no-properties)))
						    (?t . ,text-no-properties))))
			   (message "No link format match for major-mode found in `blk-insert-patterns'")))
                (message "Match has no id")))
          (message "Pattern has no `extract-id-function'")))
    (message "%s not found" text)))

(defun blk-grep (grepper patterns files)
  "Run the blk grepper on the given patterns and files,
`grepper' can be a function that takes in the pattern tables and files
as arguments; see `blk-grepper-emacs'.  Alternatively, it may be a
property list describing a shell command, see `blk-grepper-grep',"
  (if (functionp grepper)
      (funcall grepper patterns files)
    (when (listp grepper)
      (blk-run-grep-cmd grepper patterns files))))

(defun blk-find-links-to-id (id)
  "Find links that point to ID."
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
            (blk-grep blk-grepper id-patterns (blk-list-files))))))
    grep-results))

(defun blk-find-by-id (id)
  "Return the file and position of a blk link ID."
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
            (blk-grep blk-grepper id-patterns (blk-list-files))))))
    grep-results))

(require 'blk-org)

(provide 'blk)
;; blk.el ends here
