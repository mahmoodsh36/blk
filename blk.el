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

(require 'blk-org)

(defcustom blk-directories (list (expand-file-name "~/notes/")
				 (file-name-parent-directory (expand-file-name user-init-file)))
  "Blk directories within which to find files to insert and search for links.")

(defcustom blk-search-recursively nil
  "Non-nil means to invoke greppers recursively in `blk-directories'.
Default is nil; changing it may have severe consequences on speed.")

(defcustom blk-emacs-patterns
  (list
   (list :title "titled org file or block"
         :anchor-regex "\\(:title\\|:alias\\|#\\+title:\\|#\\+alias:\\|#\\+name:\\)\s+[^:]+"
         :title-function 'blk-value-after-space
         :extract-id-function 'blk-org-id-at-point
         :glob "*.org")
   (list :title "elisp function"
         :glob "*.el"
         :anchor-regex "^(defun\s+[^\s]+"
         :title-function 'blk-value-after-space)
   (list :title "org header"
         :glob "*.org"
         :anchor-regex "^\\*+ .*"
         :title-function 'blk-value-after-space
         :extract-id-function 'blk-org-id-at-point)
   (list :glob "*.org"
         :anchor-regex "^:ID:\\s*.*"
         :src-id-function 'blk-org-id-value)
   (list :glob "*.org"
         :anchor-regex org-link-any-re
         :dest-id-function 'blk-org-link-path)
   (list :glob "*.org"
         :anchor-regex "#\\+identifier:\s+.*"
         :src-id-function 'blk-value-after-colon)
   (list :title "latex label"
         :glob '("*.org" "*.tex")
         :anchor-regex "\\\\label{[^\\{\\}]*}"
         :src-id-function 'blk-latex-label-id
         :title-function 'blk-latex-label-id
         :transclusion-function 'blk-tex-transclusion-env-at-point)
   (list :title "id anchor for org named block"
         :glob "*.org"
         :anchor-regex "#\\+name:\\s+.*|:name\\s+[^:]*"
         :src-id-function 'blk-value-after-space-before-colon
         :transclusion-function 'blk-org-transclusion-at-point))
  "The pattern table for the elisp grepper; see documentation for `blk-patterns'.")

(defcustom blk-rg-patterns
  (list
   (list :title "titled org file or block"
         :glob "*.org"
         :anchor-regex "(:title|:alias|#\\+title:|#\\+alias:|#\\+name:)\\s+[^:]+"
         :title-function 'blk-value-after-space
         :extract-id-function #'blk-org-id-at-point)
   (list :title "elisp function"
         :glob "*.el"
         :anchor-regex "^\\(defun\\s+\\S+"
         :title-function 'blk-value-after-space)
   (list :title "org header"
         :glob "*.org"
         :anchor-regex "^\\*+\\s.*"
         :title-function 'blk-value-after-space
         :extract-id-function 'blk-org-id-at-point)
   (list :glob "*.org"
         :anchor-regex "^:ID:\\s*.*"
         :src-id-function 'blk-org-id-value)
   (list :glob "*.org"
         :anchor-regex "\\[\\[[a-z]+:[^\\[\\]]+\\]\\]|\\[\\[[a-z]+:[^\\[\\]]+\\]\\[[^\\[\\]]+\\]\\]"
         :dest-id-function 'blk-org-link-path)
   (list :glob "*.org"
         :anchor-regex "#\\+identifier:\\s+.*"
         :src-id-function 'blk-value-after-colon)
   (list :title "latex label"
         :glob (list "*.org" "*.tex")
         :anchor-regex "\\\\\\\\label\\\\{[^\\\\{\\\\}]*\\\\}"
         :src-id-function 'blk-latex-label-id
         :title-function 'blk-latex-label-id
         :transclusion-function 'blk-tex-transclusion-env-at-point)
   (list :title "id anchor for org named block"
         :glob "*.org"
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

(defcustom
  blk-tex-env-at-point-function
  #'blk-naive-env-at-point-bounds
  "Could be one of `blk-auctex-env-at-point-bounds', `blk-naive-env-at-point-bounds' (the former
maybe be more \"sophisticated\"). has to be a function that returns a cons of the from
(beginning . end) for the start/end position of the latex environment at point, respectively.
used for transclusion of latex environments")

(defun blk-auctex-env-at-point-bounds ()
  "Get the boundaries of the latex environment at point using auctex, errors out if
auctex isnt installed and loaded. errors out if theres no latex environment at point.
Returns a cons of the from (beginning . end) for the start/end position of the latex environment
at point, respectively."
  (save-excursion
    (LaTeX-find-matching-begin)
    (let ((begin (point)))
      (forward-char) ;; without this auctex cant find \end{} for some reason
      (LaTeX-find-matching-end)
      (cons begin (point)))))

(defun blk-naive-env-at-point-bounds ()
  "Get the boundaries of the latex environment at point using a simple regex search,
Returns a cons of the from (beginning . end) for the start/end position of the latex environment
at point, respectively. errors out if it fails to find a latex environment."
  (save-excursion
    (search-backward "\\begin{")
    (let ((begin (point)))
      (re-search-forward "\\\\end{[^{}]+}")
      (goto-char (match-end 0))
      (let ((end (point)))
        (cons begin end)))))

(defun blk-tex-transclusion-env-at-point (grep-data)
  "Function that returns the latex environment the cursor is in.
the plist returned represents an org-transclusion object which is then passed to
org-transclusion to be handled for transclusion in an org buffer.
this currently doesnt do anything being looking for the regex corresponding
to a \\begin and \\end, which isnt the smartest way of doing it, but as long
as the destination \\label{ID} is present in a latex environment the function
works as intended. syntax like \\[ \\] isnt yet handled.
the argument GREP-DATA is the result returned from the search for ID
it is unused and may be ignored, but since the function is called with it
we have to keep it defined this way.
the function makes use of `org-latex-regexps', but it doesnt necessarily
depend on org-mode as it may work outside org-mode, since org-mode is builtin
we can safely assume the variable is there.
returns a plist that is then passed to org-transclusion"
  (let ((bounds (funcall blk-tex-env-at-point-function))
        (begin (car bounds))
        (end (cdr bounds))))
  (list :src-content (buffer-substring begin end)
        :src-buf (current-buffer)
        :src-beg begin
        :src-end end))

(defun blk-tex-transclusion-env-at-point (grep-data)
  "Function that returns the latex environment the cursor is in.
the plist returned represents an org-transclusion object which is then passed to
org-transclusion to be handled for transclusion in an org buffer.
this currently doesnt do anything being looking for the regex corresponding
to a \\begin and \\end, which isnt the smartest way of doing it, but as long
as the destination \\label{ID} is present in a latex environment the function
works as intended. syntax like \\[ \\] isnt yet handled.
the argument GREP-DATA is the result returned from the search for ID
it is unused and may be ignored, but since the function is called with it
we have to keep it defined this way.
the function makes use of `org-latex-regexps', but it doesnt necessarily
depend on org-mode as it may work outside org-mode, since org-mode is builtin
we can safely assume the variable is there.
returns a plist that is then passed to org-transclusion"
  (condition-case err
      (progn (search-backward "\\begin{")
             (let ((begin (point)))
               (re-search-forward "\\\\end{[^{}]+}")
               (goto-char (match-end 0))
               (let ((end (point)))
                 (list :src-content (buffer-substring begin end)
                       :src-buf (current-buffer)
                       :src-beg begin
                       :src-end end))))
    (error nil)))

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
  ;; the 'eval' trickery is there because grep doesnt accept directories to search in,
  ;; it only accepts files, so here we're expanding the glob in the shell itself
  ;; before passing the paths to grep
  '(:command "grep -E -e \"%r\" $(eval echo $(printf ' %%s*.* ' %f)) --line-number --ignore-case --byte-offset --only-matching -d skip"
             :delimiter ":"
             :glob-arg "--include "
             :recursive-arg "-R"))

(defconst
  blk-grepper-rg
  (list :command "rg --max-depth 1 --field-match-separator '\t' --regexp \"%r\" %f --no-heading --line-number --ignore-case --byte-offset --only-matching --with-filename"
        :delimiter "\t"
        :glob-arg "--glob "
        :recursive-arg "--max-depth 10000")) ;; rg can handle a dupe arg, the latter overrides the former.

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
to grep for, together with the file list.
See the function `blk-run-grep-cmd' for what the plist needs to contain if a grep shell
command is to be used.")

(defcustom blk-patterns
  (pcase (blk-choose-grepper)
    (blk-grepper-rg blk-rg-patterns)
    (blk-grepper-grep blk-grep-patterns)
    (blk-grepper-emacs blk-emacs-patterns))
  "The list of patterns to use with the blk grepper.
Each entry should be a plist representing the data of a pattern:
  :title is the title/type of the pattern (used for completing-read)
  :filename-regex is the regex to match files to be grepped; this
    should always be an emacs regex because matching filenames is done in
    Elisp.
  :anchor-regex is the regex for matching blocks of text that contain
    the target value which is then passed to :title-function to be
    turned into the final desired value to be passed to completing-read
    to serve as the entry in the completing-read menu for the target
  :src-id-function is the function that gets the id to be used when
    creating links to the target; the need for :src-id-function over
    :title-function is that an id and a name/title for a target can be
    different, as an id can be a random sequence but a name could be
    a more memorable sequence of characters.  the function takes the matched
    value and strips unnecessary turning it into just the id,
    think \\label{my-id} -> my-id
  :title-function is used alone to make the id be the name itself.
  :transclusion-function currently is a function that should take
    the match and return an object or plist that can be handled by
    org-transclusion, this allows for easily defining custom transclusion
    functions for different patterns of text.  see the function
    `blk-org-trancslusion-at-point' for an example")

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

(defun blk-grepper-emacs (pattern-table directories)
  "Function that reads patterns from PATTERN-TABLE and greps for them in DIRECTORIES."
  (let ((results))
    (dolist (pattern pattern-table)
      (let ((globs (plist-get pattern :glob)))
        (when (atom globs)
          (setq globs (list globs)))
        (let* ((files (apply 'append (mapcar (lambda (glob) (blk-list-files directories glob)) globs)))
               (all-files
                (delete-dups
                 (append
                  (apply
                   'append
                   (mapcar
                    (lambda (glob)
                      (mapcar 'buffer-file-name (blk-list-buffers glob)))
                    globs))
                  files))))
          (dolist (filepath all-files)
            (let ((buf (find-buffer-visiting filepath)))
              ;; if file isnt already opened in some buffer, we open it ourselves, we dont use `find-file-noselect' as using `insert-file-contents' makes the code run alot faster
              (when (not buf)
                (setq buf (get-buffer-create " blk"))
                (with-current-buffer buf
                  (delete-region (point-min) (point-max))
                  (insert-file-contents filepath)))
              (with-current-buffer buf
                (let ((matches (blk-string-search-regex (plist-get pattern :anchor-regex)
                                                        (substring-no-properties (buffer-string)))))
                  (dolist (match matches)
                    (push (list :position (1+ (cdr match))
                                :filepath filepath
                                :matched-value (car match)
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

(defun blk-list-files (directories glob)
  "Return a list of files in `blk-directories' for grepping links/references.
For when `blk-grepper-emacs' is used.
Recurse subdirectories if `blk-search-recursively' is non-nil."
  (let ((files))
    (dolist (dir directories)
      (setq files
            (append files
                    (if blk-search-recursively
                        (mapcar (lambda (filename) (expand-file-name filename dir))
                                (directory-files-recursively dir (wildcard-to-regexp glob)))
                      (directory-files dir t (wildcard-to-regexp glob))))))
    files))

(defun blk-list-buffers (glob)
  "List buffers matching GLOB for searching when `blk-grepper-emacs' is used."
  (cl-remove-if-not
   (lambda (buf)
     (when (buffer-file-name buf)
       (string-match-p (wildcard-to-regexp glob) (buffer-file-name buf))))
   (buffer-list)))

(defun blk-list-entries ()
  "List all the pattern matches found in the blk files."
  (let* ((grep-results
          (cl-remove-if-not
           'identity
           (mapcar
            (lambda (grep-result)
              (let* ((title-func (plist-get (plist-get grep-result :matched-pattern) :title-function))
                     (matched-value (plist-get grep-result :matched-value))
                     (title (funcall title-func matched-value)))
                (if (not (string-empty-p (string-trim title)))
                    (plist-put grep-result
                               :title      ;; :title of match not to be confused with :title of the matched pattern
                               title)
                  nil)))
            (blk-grep
             blk-grepper
             (cl-remove-if-not (lambda (pattern) (plist-get pattern :title-function)) blk-patterns)
             blk-directories))))
         (entries (mapcar (lambda (grep-result)
                            (propertize (plist-get grep-result :title) 'grep-data grep-result))
                          grep-results)))
    entries))

(defun blk-str-list-matches (regex str-list)
  "Return a list of the strings matching REGEX in STR-LIST."
  (cl-remove-if-not
   (lambda (str)
     (string-match regex str))
   str-list))

(defun blk-run-grep-cmd (cmd patterns directories)
  "Run a grep-like CMD matching any PATTERNS across a list of DIRECTORIES.
Return a list of lists of key-value pairs of the form:
  '(:matched-value <value>
    :position <buffer-position>
    :line-number <line-number>
    :matched-pattern <grep-pattern>
    :filepath <filepath>).
CMD is a plist where :command is the shell command and :delimiter is the delimiter in the output,
:glob-arg is the argument of the grep command that takes a glob for filename matching,
see `blk-grepper-grep' as an example of CMD.
The result of CMD should contain lines of the form [filepath]<sep>[line]<sep>[position]<sep>[match] where
sep is the property :delimiter of the plist CMD"
  (let* (full-cmd
         matches
         out
         sep
         line-entries
         filepath
         line-number
         position
         match-text
         got-error
         globs
         exit-code
         (glob-arg (plist-get cmd :glob-arg))
         (recursive-arg (plist-get cmd :recursive-arg))
         (bfr-name " blk-out") ;; we use this (internal) buffer to grab the results of call-process
         (bfr (get-buffer-create bfr-name)))
    (dolist (pattern patterns)
      (when (not got-error)
        (with-current-buffer bfr
          (delete-region (point-min) (point-max)))
        (setq globs (plist-get pattern :glob))
        (when (atom globs)
          (setq globs (list globs)))
        (setq globs-str (when glob-arg
                          (string-join (mapcar (lambda (glob) (concat glob-arg (shell-quote-argument glob))) globs) " "))
              files-str
              (string-join
               (mapcar
                (lambda (dirpath)
                  (if glob-arg
                      (format "%s/" dirpath)
                    (string-join ;; if glob-arg isnt provided, we expand the globs (i.e. wildcards) ourselves
                     (mapcar
                      (lambda (glob)
                        (string-join
                         (mapcar 'shell-quote-argument
                                 (file-expand-wildcards (format "%s/%s" dirpath glob)))
                         " "))
                      globs)
                     " ")))
                directories)
               " ")
              full-cmd (format
                        "%s %s%s"
                        (format-spec (plist-get cmd :command)
                                    `((?f . ,files-str)
                                      (?r . ,(plist-get pattern :anchor-regex))))
                        globs-str
                        (if (and recursive-arg blk-search-recursively) recursive-arg ""))
              exit-code (call-process-shell-command full-cmd nil bfr-name)
              out (with-current-buffer " blk-out" (substring-no-properties (buffer-string)))
              sep (plist-get cmd :delimiter))
        (if (or (equal exit-code 0) (equal exit-code 1)) ;; i think exit-code 1 is usually for no match
            (dolist (line (split-string out "\n"))
              (when (not (string-empty-p line))
                (setq line-entries (split-string line sep)
                      filepath (car line-entries)
                      line-number (string-to-number (or (cadr line-entries) "1"))
                      position (string-to-number (or (caddr line-entries) "0"))
                      match-text (if (cdddr line-entries) (string-join (cdddr line-entries) sep) ""))
                (push (list :matched-value match-text
                            :position (1+ position) ;; grep starts at position 0, while emacs doesnt
                            :line-number line-number
                            :matched-pattern pattern
                            :filepath filepath)
                      matches)))
          (progn
            (message "received exit code %s with error: %s" exit-code out)
            (setq got-error t)))))
    (when (not got-error)
        matches)))

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
           (when entries (completing-read "entry " entries)))))
  (when text
    (if (get-text-property 0 'grep-data text)
        (let* ((grep-data (get-text-property 0 'grep-data text))
               (filepath (plist-get grep-data :filepath))
               (position (plist-get grep-data :position)))
          (find-file filepath)
          (goto-char position))
      (message "%s not found" text))))

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
        ;; if :extract-id-function isnt provided, we could try making our own that simply
        ;; returns the "src id" of the target to be linked to, although notice that if this is to
        ;; happen, the file might be later loaded into memory for no reason by `blk-with-file-as-current-buffer'
        (when (not extract-id-func)
          (let ((src-id-func (plist-get (plist-get grep-data :matched-pattern) :src-id-function)))
            (when src-id-func
                (setq extract-id-func
                      (lambda (grep-data-local)
                        ;; `grep-data-local' would be the same as `grep-data' anyway
                        (funcall src-id-func (plist-get grep-data-local :matched-value)))))))
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
          (message "Pattern has no `extract-id-function' or `src-id-function'")))
    (message "%s not found" text)))

(defun blk-grep (grepper patterns directories)
  "Run the blk grepper on the given patterns and directories,
`grepper' can be a function that takes in the pattern tables and files
as arguments; see `blk-grepper-emacs'.  Alternatively, it may be a
property list describing a shell command, see `blk-grepper-grep',"
  (if (functionp grepper)
      (funcall grepper patterns directories)
    (when (listp grepper)
      (blk-run-grep-cmd grepper patterns directories))))

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
                                  (plist-get grep-result :matched-value))))
            (blk-grep blk-grepper id-patterns blk-directories)))))
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
                                  (plist-get grep-result :matched-value))))
            (blk-grep blk-grepper id-patterns blk-directories)))))
    grep-results))

(defun blk-collect-all ()
  "Collect some data about the text files that we know of."
  (mapcar
   (lambda (grep-result)
     (let* ((pattern (plist-get grep-result :matched-pattern))
            (title-func (plist-get pattern :title-function))
            (matched-value (plist-get grep-result :matched-value))
            (src-id-func (plist-get pattern :matched-value)))
       (when title-func
         (plist-put grep-result
                    :title
                    (funcall title-func matched-value)))
       (when src-id-func
         (plist-put grep-result
                    :id
                    (funcall src-id-func matched-value)))
       grep-result)
     grep-result)
   (blk-grep blk-grepper
             blk-patterns
             blk-directories)))

;;;###autoload
(defun blk-all-to-json (filepath)
  "Export the data recognizable by blk into a json file"
  (interactive (list (read-file-name "output file: ")))
  (if (json-available-p)
      (let* ((data (blk-collect-all)))
        (with-temp-file
            filepath
          (insert (json-encode data)))
        (message "Wrote json to %s" filepath))
    (message "Json isnt available")))

(provide 'blk)
;; blk.el ends here
