;;; emacs/dired/autoload.el -*- lexical-binding: t; -*-

(defvar rename-regexp-query nil)

;;;###autoload
(defun +dired-enable-git-info-h ()
  "Enable `dired-git-info-mode' in git repos."
  (and (not (file-remote-p default-directory))
       (locate-dominating-file "." ".git")
       (dired-git-info-mode 1)))

;;;###autoload
(defun +jg-dired-touch (target)
  (interactive "MTouch File: \n")
  (call-process "touch" nil nil nil (f-join default-directory target))
  )

;;;###autoload
(defun +jg-dired-rename ()
  (interactive)
  (let ((dired-dwim-target nil))
    (dired-do-rename)
    )
  )

;;;###autoload
(defun +jg-dired-hash-files ()
  (interactive)
  (let* ((marked (dired-get-marked-files))
         (quoted (mapcar #'shell-quote-argument marked))
         uniqs
         dups
         )
    (with-current-buffer (get-buffer-create jg-hash-check-buffer) (erase-buffer))
    (shell-command (format jg-hash-check-command (s-join " " quoted)) jg-hash-check-buffer)
    (setq uniqs (split-string (with-current-buffer jg-hash-check-buffer (buffer-string)) "\n" " ")
          dups (-difference marked uniqs))
    (dired-unmark-all-marks)
    (dired-mark-if (and (dired-get-filename nil t) (-contains? dups (dired-get-filename nil t))) "Duplicated SHA Checksum")
    (message "%s Duplicates" (length dups))
    )
  )

;;;###autoload
(defun +jg-dired-kill-subdir-or-close-buffer ()
  (interactive)
  (condition-case err
      (dired-kill-subdir)
    (error
     (cl-assert (string-equal "Attempt to kill top level directory" (cadr err)))
     (kill-current-buffer)
     )
    )
  )

;;;###autoload
(defun +jg-dired-GLOBAL-do-rename-regexp (regexp newname arg whole-name)
  "Customised dired-do-rename-regexp from dired-aux to use GLOBAL regexp flag

Rename selected files whose names match REGEXP to NEWNAME.
"
  (interactive (dired-mark-read-regexp "Rename"))
  (+jg-dired-GLOBAL-do-create-files-regexp
   #'dired-rename-file
   "Rename" arg regexp newname dired-keep-marker-rename))

;;;###autoload
(defun +jg-dired-GLOBAL-do-create-files-regexp
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

;;;###autoload
(defun +jg-dired-find-random-marked-file ()
  "Open random file from marked"
  (interactive)
  (let ((marked (dired-get-marked-files)))
    (find-file (nth (random (length marked))
                    marked))
    )
  )

;;;###autoload
(defun +jg-dired-insert-subdir-maybe-recursive (&optional switches)
  "Insert Subdir tree in dired"
  (interactive (list (if current-prefix-arg +jg-dired-recursive-switches)))

  (let ((marked-files (dired-get-marked-files))
        (current-prefix-arg nil)
        )
    (cl-loop for file in marked-files
             do
             (dired-maybe-insert-subdir file switches)
             )
    )
  )

;;;###autoload
(defun +jg-dired-insert-marked-subdir ()
  (interactive)
  (let ((marked (-filter #'f-directory? (dired-get-marked-files))))
    (mapc #'dired-maybe-insert-subdir marked)
    )
  )

;;;###autoload
(defun +jg-dired-diff ()
  "Diff Files from Dired"
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (if (eq 2 (length marked-files))
        (ediff (car marked-files) (cadr marked-files))
      (message "Mark only 2 files")
      )
    )
  )

;;;###autoload
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

;;;###autoload
(defun +jg-dired-marked-info ()
  "Count marked files"
  (interactive)
  (message "%s files are marked" (length (dired-get-marked-files)))
  )

;;;###autoload
(defun +jg-dired-dired-auto-move ()
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

;;;###autoload
(defun +jg-dired-find-literal ()
  (interactive)
  (find-file-literally (dired-get-filename))
  )

;;;###autoload
(defun +jg-dired-epa-list-keys ()
  (interactive)
  (epa-list-keys)
  )

;;;###autoload
(defun +jg-dired-epa-export-keys (prefix)
  (interactive "P")
  (let* ((context (epg-make-context epa-protocol epa-armor))
         (keys (epa-select-keys context "Select Keys to Export"))
         (file (read-file-name "To File: ")))
    (if (not prefix)
        (epa-export-keys keys file)
      (progn
        (setf (epg-context-output-file context) file)
        (setf (epg-context-operation context) 'export-keys)
        (setf (epg-context-result context) nil)
        (epg--start context (cons "--export-secret-keys"
			          (mapcar
			           (lambda (key)
			             (epg-sub-key-id
			              (car (epg-key-sub-key-list key))))
			           keys)))
	(epg-wait-for-completion context)
        (epg-reset context))
      )
    )
)
