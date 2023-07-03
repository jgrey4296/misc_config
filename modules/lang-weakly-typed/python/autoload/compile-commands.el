;;; compile-commands.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-python-get-commands (&optional dir)
  (interactive)
  (-when-let* ((root (projectile-project-root dir))
               (project (f-join root "pyproject.toml"))
               (project-exists (f-exists? project))
               )
    (append
     ;; pip
     (+jg-projects-annotate-cmds
      '(

        )
      #'(lambda (x) (concat "pip" " " (car (split-string x" " t " "))))
      )
     ;; pytest
     (+jg-projects-annotate-cmds
      '()
      #'(lambda (x) (concat "pytest" " " (car (split-string x" " t " "))))
      )
     )
    )
  )
