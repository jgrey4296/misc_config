;; tree-views.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-ui-treemacs-expand-dir (&optional arg)
  (interactive "P")
  (treemacs-do-for-button-state
   :on-root-node-closed (treemacs--expand-root-node btn arg)
   :on-dir-node-closed  (treemacs--expand-dir-node btn :recursive arg)
   :on-file-node-closed (treemacs--expand-file-node btn arg)
   :on-tag-node-closed  (treemacs--expand-tag-node btn arg)
   :on-nil              (treemacs-pulse-on-failure "There is nothing to do here.")
   :fallback            #'ignore
   )
  )

;;;###autoload
(defun +jg-ui-tree/open ()
  "Open the the current tree package window in the current project."
  (interactive)
  (pcase jg-ui-tree-active-tree-package
    (treemacs
     (require 'treemacs)
     (treemacs)
     )
    (neotree
     (require 'neotree)
     (neotree-dir (or (projectile-project-root) default-directory))
     )
    (x
     (user-error "Unknown Tree Package: %s" x)
     )
    )
  )

;;;###autoload
(defun +jg-ui-tree/find-this-file ()
  "Open the neotree window in the current project, and find the current file."
  (interactive)
  (let ((path buffer-file-name)
        (project-root (or (projectile-project-root) default-directory)))
    (pcase jg-ui-tree-active-tree-package
      (treemacs
       (treemacs-find-file)
       )
      (neotree
       (require 'neotree)
       (cond ((and (neo-global--window-exists-p)
                   (get-buffer-window neo-buffer-name t))
              (neotree-find path project-root)
              (neotree-refresh))
             ((not (and (neo-global--window-exists-p)
                        (equal (file-truename (neo-global--with-buffer neo-buffer--start-node))
                               (file-truename project-root))))
              (neotree-dir project-root)
              (neotree-find path project-root))
             (t
              (neotree-find path project-root)))
       )
      (x
       (user-error "Unknown Tree Package: %s" x)
       )
      )
    )
  )
