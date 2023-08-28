;;; +related.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-projects-find-related ()
  (interactive)
  (-when-let* ((buff (if (eq major-mode 'dired-mode)
                       (+jg-projects-find-related-directory)
                       (projectile--find-related-file (buffer-file-name))))
               (buff-exists (f-exists? buff))
               (wind-fn (if (boundp 'jg-workspaces-find-buff-fn)
                            jg-workspaces-find-buff-fn
                          #'+jg-workspace-default-new-window))
               )
    (funcall wind-fn buff)
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

(defun +jg-workspace-default-new-window (buff)
  (select-window (split-window-below))
  (find-file buff)
  )
