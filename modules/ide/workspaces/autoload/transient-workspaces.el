;;; transient.el -*- lexical-binding: t; -*-
(require 'macro-tools--transient)

(defvar jg-workspace-transient-hook nil)

(defun +jg-workspace-settings-group-title ()
  (format "Buffer: %s\nProject: %s\n"
          (window-buffer (selected-window))
          (projectile-project-root)
          )
  )
;; Projects
(transient-call! magit-todos ()
  "Todos"
  :interactive t
  :key "t"
  :desc (macro-tools--transient-simple-fmt "Todos" "t")
  :transient nil
  #'magit-todos-list
  )
(transient-call! workspaces-ivy ()
  "Workspaces"
  :key "RET"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Workspaces" "RET")
  :transient nil
  #'+jg-workspaces-ivy
  )

;;;###autoload
(defun +jg-workspace-run-transient ()
  (interactive)
  (let ((transient--buffer-name jg-workspaces-transient-buffer-name))
    (workspace-control-transient)
    )
  )

;;;###autoload (autoload 'workspaces-transient-builder "ide/workspaces/autoload/transient-workspaces")
(transient-setup-hook! workspaces-transient ()
  (transient-define-prefix workspace-control-transient ()
    "The main workspace control transient"
    [:description +jg-workspace-settings-group-title
                  ["|| General ||"
                   (transient-macro-call-workspaces-ivy)
                   (transient-macro-call-magit-todos)
                   ]
                  ]
    []
    macro-tools--transient-quit!
    )
  )
