;;; polyglot.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;+jg-eval--pair-cmds

(defun +jg-eval-polyglot-get-commands (&optional dir)
  (interactive)
  (-when-let* ((root (projectile-project-root dir))
               (proj-name (projectile-project-name))
               (proj-src (-if-let* ((loc (f-join root proj-name))
                                    (exists (f-exists? loc))
                                    )
                             loc
                           root))
               (project (f-join root "pyproject.toml"))
               (project-exists (f-exists? project))
               (curr-file (buffer-file-name))
               )
  ;; check for polyglot env var

  ;; list tasks

  ;; list tools

  ;; list langs

    )
  )


;;; polyglot.el ends here
