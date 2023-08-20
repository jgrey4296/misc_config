;;; ui/workspaces/autoload/ivy.el -*- lexical-binding: t; -*-
(require 'counsel)
(require 'ivy)

;;;###autoload
(defun +workspace--ivy-rich-preview (workspace)
  (if-let (buffers (when-let (workspace (gethash workspace *persp-hash*))
                     (cl-loop for (type . rest) in (persp-window-conf workspace)
                              if (eq type 'buffer)
                              collect (car leaf)
                              else if (eq type 'leaf)
                              append (cl-loop for (type . leaf) in rest
                                              if (eq type 'buffer)
                                              collect (car leaf)))))
      (string-join buffers " ")
    "*No buffers*"))

;;;###autoload
(defun +jg-workspaces-rename (x)
  (+workspace-rename x (read-string (format "Rename %s -> : " x)))
  )

;;;###autoload
(defun +jg-workspaces-new-ring (x)
  (carousel-new)
  )

(defun +jg-workspaces-switch (x)
  (cond ((string-equal x (+workspace-current-name))
         (+workspace-save x))
        ((+workspace-exists-p x)
         (+workspace-switch x))
        ((-contains? (bookmark-all-names) x)
         (+workspace-switch x t)
         (bookmark-jump x))
        (t (+workspace-switch x t))
        )
  )

;;;###autoload
(defun +jg-workspaces-ivy ()
    "Switch to a workspace or create a new one"
    (interactive)
    (require 'bookmark)
    (ivy-read "Create or jump to workspace: "
              (+workspace-list-names)
              :history 'workspace-history
              :action #'+jg-workspaces-switch
              :caller '+jg-workspaces-ivy)
    )

;;;###autoload
(defun +jg-workspaces-get-doot-commands (&optional dir)
  " get tasks from doot, cache them, then wrap them for use in ivy "
  ;; add to counsel-compile-local-builds
  (interactive)
  (-when-let* ((root (projectile-project-root dir))
               (doot-toml (f-join root "doot.toml"))
               (dt-exists (f-exists? doot-toml))
               )
    (+jg-projects-doot-tasks)
    )
  )

;;; ivy.el ends here
