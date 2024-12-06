;;; transient.el -*- lexical-binding: t; -*-
(require 'transient)

(defvar jg-workspace-transient-hook nil)

(defun +jg-workspace-settings-group-title ()
  (format "Buffer: %s" (window-buffer (selected-window)))
  )
(progn
  ;; Projects
  (transient-make-call! goto-root                "`"
                        "Goto-Root"
                        :transient nil
                        (find-file (doom-project-root)))
  (transient-make-int-call! magit-todos         "t"
                            "Todos"
                            :transient nil
                            #'magit-todos-list)
  (transient-make-int-call! workspaces-ivy      "RET"
                            "Workspaces"
                            :transient nil
                            #'+jg-workspaces-ivy)
  )

;;;###autoload
(defun +jg-workspace-build-workspace-transient()
  (interactive)
  (transient-define-prefix workspace-control-transient ()
    "The main workspace control transient"
    [:description +jg-workspace-settings-group-title
                  ["|| General ||" (transient-macro-call-goto-root)]
                  ["" (transient-macro-call-magit-todos)]
                  ["" (transient-macro-call-workspaces-ivy)]
                  ]
    []
    transient-quit!
    )

  (run-hooks 'jg-workspace-transient-hook)
)

;;;###autoload
(defun +jg-workspace-run-transient ()
  (interactive)
  (let ((transient--buffer-name jg-workspaces-transient-buffer-name))
    (workspace-control-transient)
    )
  )
