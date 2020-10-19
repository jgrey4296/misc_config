;;; -*- lexical-binding: t; -*-

;; A Simple minor mode to walk through every file in a project

(defun project-walk-init ()
  (interactive)
  (setq project-walk-list (projectile-project-files (projectile-project-root)))
  )

(defun project-walk-filter-p (filename)
  " Reject if on the blacklist, or extention doesn't match "
  ;; contain check taken outside cond because it would always use first condition for some reason
  (let ((in-filter-list (-contains? project-walk-filter-list (f-filename filename)))
        (name-match (if (not project-walk-filter-name) nil (s-contains? project-walk-filter-name (f-filename filename))))
        (dir-match (if (not project-walk-filter-dir) nil (s-contains? project-walk-filter-dir (f-dirname filename))))
        (is-dot-file (s-prefix? "." filename))
        )
    )
    (cond (in-filter-list nil)
          (name-match nil)
          (dir-match nil)
          (is-dot-file nil)
          (project-walk-ext (not (s-equals? (f-ext filename) project-walk-ext)))
          (t t)
          )
    )
  )


(defun project-walk-filter (arg)
  (interactive "p")
  (cond ((eq arg 0) (message "Reminder: 1: Normal, 2: Filter Name, 3: Filter Ext, 4: Filter Dir"))
        ((eq arg 2) (setq project-walk-filter-name (read-string "Filename Part to Filter: ")))
        ((eq arg 3) (setq project-walk-ext (read-string "File ext to Ignore: ")))
        ((eq arg 4) (setq project-walk-filter-dir (read-string "Dir Part to Filter: ")))
        )
  (if (not (eq 0 arg))
      (setq project-walk-list (-filter 'project-walk-filter-p project-walk-list)))
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
  (message "%s" project-walk-list)
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
                project-walk-ext nil
                project-walk-filter-list '("__init__.py")
                project-walk-filter-name nil
                project-walk-filter-dir nil
                )

  (project-walk-init)
)

(provide 'project-walk)
