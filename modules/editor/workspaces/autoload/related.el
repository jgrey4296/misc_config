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
  (message "Finding Related Directories")
  (let ((root (projectile-project-root))
        (proj-type (projectile-project-type))
        (proj (alist-get (projectile-project-type) projectile-project-types))
        )

    )
  )
)
