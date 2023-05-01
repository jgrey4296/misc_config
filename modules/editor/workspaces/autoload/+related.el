;;; +related.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-projects-find-related ()
  (interactive)
  (if (eq major-mode 'dired-mode)
      (+jg-projects-find-related-directory)
    (projectile-find-related-file)
    )
  )

(defun +jg-projects-find-related-directory ()
  (when (eq (projectile-project-type) 'jg-toml-project)
    (message "Finding Related Directories")

    )
  )
