;;; emacs/dired/+funcs.el -*- lexical-binding: t; -*-

(defvar rename-regexp-query nil)

(defun +jg-GLOBAL-dired-do-rename-regexp (regexp newname arg whole-name)
  "Customised dired-do-rename-regexp from dired-aux to use GLOBAL regexp flag

Rename selected files whose names match REGEXP to NEWNAME.
"
  (interactive (dired-mark-read-regexp "Rename"))
  (+jg-GLOBAL-dired-do-create-files-regexp
   #'dired-rename-file
   "Rename" arg regexp newname dired-keep-marker-rename))

(defun +jg-GLOBAL-dired-do-create-files-regexp
  (file-creator operation arg regexp newname &optional marker-char)
  " Create a new file for each marked file using regexps.
FILE-CREATOR and OPERATION as in dired-create-files.
ARG as in dired-get-marked-files.
Matches each marked file against REGEXP and constructs the new
filename from NEWNAME (like in function replace-match).
Optional arg WHOLE-NAME means match/replace the whole file name
instead of only the non-directory part of the file.
Optional arg MARKER-CHAR as in dired-create-files.
"
  (let* ((fn-list (dired-get-marked-files nil arg))
	 (operation-prompt (concat operation " `%s' to `%s'?"))
	 (rename-regexp-help-form (format-message "\
Type SPC or `y' to %s one match, DEL or `n' to skip to next,
`!' to %s all remaining matches with no more questions."
						  (downcase operation)
						  (downcase operation)))
	 (regexp-name-constructor
	  ;; Function to construct new filename using REGEXP and NEWNAME:
	    ;; not whole-name, replace non-directory part only
	    (lambda (from)
	      (let* ((new (dired-string-replace-match regexp (file-name-nondirectory from) newname nil t))
		     (to (and new	; nil means there was no match
			      (expand-file-name new
						(file-name-directory from))))
		     (help-form rename-regexp-help-form))
		(if to
		    (and (dired-query 'rename-regexp-query
				      operation-prompt
				      (dired-make-relative from)
				      (dired-make-relative to))
			 to)
		  (dired-log "%s: %s did not match regexp %s\n"
			     operation (file-name-nondirectory from) regexp)))))
	 rename-regexp-query)
    (dired-create-files
     file-creator operation fn-list regexp-name-constructor marker-char)))

(defun +jg-dired-find-random-marked-file ()
  "Open random file from marked"
  (interactive)
  (let ((marked (dired-get-marked-files)))
    (find-file (nth (random (length marked))
                    marked))
    )
  )

(defun +jg-dired-insert-subdir-maybe-recursive (dirname &optional switches)
  "Insert Subdir tree in dired"
  (interactive
   (list (dired-get-filename)
         (if current-prefix-arg +jg-personal-dired-recursive-switches)))
  (let ((current-prefix-arg nil))
    (dired-maybe-insert-subdir dirname switches))
  )

(defun +jg-dired-diff ()
  "Diff Files from Dired"
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (if (eq 2 (length marked-files))
        (diff (car marked-files) (cadr marked-files))
      (message "Mark only 2 files")
      )
    )
  )

(defun +jg-dired-create-summary-of-orgs ()
  "Index org files subtree of cwd"
  (interactive)
  (let* ((cwd (dired-current-directory))
         (org-files (directory-files-recursively cwd "\.org")))
    (with-output-to-temp-buffer "*CWD Index*"
      (set-buffer "*CWD Index*")
      (insert (format "* Index of: %s\n" cwd))
      (mapc (lambda (x) (insert (format "** [[%s][%s]]\n" x (f-base x)))) org-files)
      (org-mode)
      (goto-char (point-min))
      (org-sort-entries nil ?a)
      )
    )
  )

(defun +jg-dired-marked-info ()
  "Count marked files"
  (interactive)
  (message "%s files are marked" (length (dired-get-marked-files)))
  )

(defun +jg-personal-dired-auto-move ()
  " Auto up directory then down line"
  (interactive)
  (dired-up-directory)
  (evil-next-line)
  (dired-find-file)
  ;; TODO
  (debug)
  ;;(spacemacs/open-file-or-directory-in-external-app nil)
  (let* ((current-dir (dired-current-directory))
         (lst (dired-split "/" current-dir))
         (search-str (apply 'f-join (-take-last 3 lst))))
    (evil-window-right 1)
    (goto-char (point-min))
    (message "Searching for %s" search-str)
    (search-forward search-str)
    (evil-scroll-line-to-center (line-number-at-pos (point)))
    (search-forward "tags = {")
    )
  )

(defun +jg-dired-find-literal ()
  (interactive)
  (find-file-literally (dired-get-filename))
  )
