;;; ../../../Volumes/documents/github/emacs_files/packages/project-walk/project-walk.el -*- lexical-binding: t; -*-

;; A Simple minor mode to walk through every file in a project

(defun project-walk-init ()
  (interactive)
  (setq (projectile-project-files (projectile-project-root)))
  )

(defun project-walk-filter ()
  (interactive)
  ;; TODO filter by extension
  ;; filter dot files
  ;; filter __init__ files (optional)

)

(defun project-walk-next ()
  (interactive)
  (if project-walk-list
      (find-file (pop project-walk-list))
    (message "No More Files in Project to Walk: %s" (projectile-project-root))
    )
  )


(define-minor-mode project-walk-minor-mode
  " A minor mode to walk through all "
  :lighter "Project-Walk"
  :global t
  (setq-default project-walk-list nil)
  (project-walk-init)
)

(provide 'project-walk)
