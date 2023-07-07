;;; compile-commands.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-python-get-commands (&optional dir)
  (interactive)
  (-when-let* ((root (projectile-project-root dir))
               (project (f-join root "pyproject.toml"))
               (project-exists (f-exists? project))
               )
    (append
     ;; python
     (+jg-projects-pair-cmds
      '("version" "python -version")
      )
     ;; pip
     (+jg-projects-pair-cmds
      '("version" "python -version")
      )
     ;; conda
     (+jg-projects-pair-cmds
      '("version" "python -version")
      )
     ;; pytest
     (+jg-projects-pair-cmds
      '("test" "pytest")
      )
     )
    )
  )
