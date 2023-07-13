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
      '("version" "python --version")
      )
     ;; pytest
     (+jg-projects-pair-cmds
      '("test" "pytest")
      )
     )
    )
  )

;;;###autoload
(defun +jg-python-solo-file-run (&optional dir)
  (interactive)
  (-when-let (is-py (f-ext? (buffer-file-name) "py"))
    (+jg-projects-pair-cmds
     `("run-py" ,(format "python -X dev %s" (buffer-file-name)) :interactive)
     `("run-py-verbose" ,(format "python -X dev -i -v %s" (buffer-file-name)) :interactive)
     `("run-ipy" ,(format "ipython -X %s" (buffer-file-name)))
      '("which-py" "which python")
      `("pytest-file" ,(format "pytest --pdb %s" (buffer-file-name)) :interactive)
      )
  )
)
