;;; -*- lexical-binding: t; -*-

;; A Simple minor mode to walk through every file in a project

(defun project-walk-init ()
  (interactive)
  (let* ((current (projectile-project-root))
         (files (-map #'(lambda (x) (f-join current x))
                      (projectile-project-files current))))
    (setq project-walk-list files)
    )
  )

(defun project-walk-directory-init ()
  (interactive)
  (let* ((current (helm-current-directory))
         (files (-map #'(lambda (x) (f-join current x))
                      (projectile-project-files current))))
    (setq project-walk-list files)
    )
  )

(defun project-walk-filter-defaults-p (filename)
  " Reject if on the blacklist, or the blacklist regexp "
  (let ((in-filter-list (-contains? project-walk-filter-default-exclusions (f-filename filename)))
        (regexp-pass (s-matches? project-walk-filter-default-regexp filename))
        )
    (or in-filter-list regexp-pass)
    ))
(defun project-walk-filter-name-p (filename)
  (s-contains? project-walk-filter-arg (f-filename filename)))
(defun project-walk-filter-ext-p (filename)
  (s-equals? (f-ext filename) project-walk-filter-arg))
(defun project-walk-filter-dir-p (filename)
  (s-contains? project-walk-filter-arg (f-dirname filename)))
(defun project-walk-keep-p (filename)
  (s-contains? project-walk-filter-name (f-filename filename)))

(defun project-walk-regexp-p (filename)
  (s-matches? project-walk-filter-arg filename)
  )

(defun project-walk-filter-defaults ()
  (interactive)
  (setq project-walk-list (-remove #'project-walk-filter-defaults-p project-walk-list))
)
(defun project-walk-filter-name (arg)
  (interactive "MRemove Filenames: ")
  (setq project-walk-filter-arg arg)
  (setq project-walk-list (-remove #'project-walk-filter-name-p project-walk-list))
)
(defun project-walk-filter-ext (arg)
  (interactive "MRemove Extensions: ")
  (setq project-walk-filter-arg arg)
  (setq project-walk-list (-remove #'project-walk-filter-ext-p project-walk-list))
  )
(defun project-walk-filter-dir (arg)
  (interactive "MRemove Directories: ")
  (setq project-walk-filter-arg arg)
  (setq project-walk-list (-remove #'project-walk-filter-dir-p project-walk-list))
  )
(defun project-walk-filter-keep (arg)
  (interactive "MKeep filenames: ")
  (setq project-walk-filter-arg arg)
  (setq project-walk-list (-filter #'project-walk-keep-p project-walk-list))
  )

(defun project-walk-filter-keep-regexp (arg)
  (interactive "MRegexp to keep: ")
  (setq project-walk-filter-arg arg)
  (setq project-walk-list (-filter #'project-walk-regexp-p project-walk-list))
  )
(defun project-walk-filter-regexp (arg)
  (interactive "MRegexp to filter: ")
  (setq project-walk-filter-arg arg)
  (setq project-walk-list (-remove #'project-walk-regexp-p project-walk-list))
)

(defun project-walk-next ()
  (interactive)
  (if project-walk-list
      (let ((filename (pop project-walk-list)))
        (message "Walk List: %s : %s" (length project-walk-list) filename)
        (find-file (f-join (projectile-project-root) filename)))
    (message "No More Files in Project to Walk: %s" (projectile-project-root))
    )
  )

(defun project-walk-remaining ()
  (interactive)
  (with-temp-buffer-window "*Project Walk List*" 'display-buffer-pop-up-window nil
    (mapc #'(lambda (x) (princ (format "%s\n" x))) project-walk-list)
    )
  )

(defun project-walk-num ()
  (interactive)
  (message "Remaining in %s: %s" (projectile-project-root) (length project-walk-list))
  )

(define-minor-mode project-walk-minor-mode
  " A minor mode to walk through all "
  :lighter "Project-Walk"
  :global t
  (setq-default project-walk-list nil
                project-walk-filter-default-exclusions '("__init__.py")
                project-walk-filter-default-regexp (rx line-start ?.)
                project-walk-filter-arg nil
                )

  (project-walk-init)
)

(provide 'project-walk)
