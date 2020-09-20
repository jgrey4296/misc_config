;;; main/jg-personal/+dired-funcs.el -*- lexical-binding: t; -*-


(after! dired
  (defun +jg-personal-dired-create-summary-of-orgs ()
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

  (defun +jg-personal-dired-marked-info ()
    "Count marked files"
    (interactive)
    (message "%s files are marked" (length (dired-get-marked-files)))
    )

  (defun +jg-personal-dired-insert-subdir-maybe-recursive (dirname &optional switches)
    "Insert Subdir tree in dired"
    (interactive
     (list (dired-get-filename)
           (if current-prefix-arg +jg-personal-dired-recursive-switches)))
    (let ((current-prefix-arg nil))
      (dired-maybe-insert-subdir dirname switches))
    )

  (defun +jg-personal-dired-diff ()
    "Diff Files from Dired"
    (interactive)
    (let ((marked-files (dired-get-marked-files)))
      (if (eq 2 (length marked-files))
          (diff (car marked-files) (cadr marked-files))
        (message "Mark only 2 files")
        )
      )
    )
  )
