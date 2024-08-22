;;; transient.el -*- lexical-binding: t; -*-
(require 'transient)

(defun +jg-workspace-settings-group-title ()
  (format "Buffer: %s" (window-buffer (selected-window)))
  )
(transient-make-int-call! magit-todos         "t"   "Todos"             :transient nil #'magit-todos-list)

;;-- workspace
(progn
  ;; Projects
  (transient-make-call! goto-root                "`" "Goto-Root"                 :transient nil (find-file (doom-project-root)))
  (transient-make-call! debug-project-type       "?" "Debug Project Type"        (+jg-projects-detect-type))
  (transient-make-call! recent-files             "r" "Project Recent Files"      :transient nil (projectile-recentf))

  )

;;;###autoload
(defun jg-workspace-define-workspace-control ()
  (transient-define-prefix workspace-control-transient ()
  "The main workspace control transient"
  [:description +jg-workspace-settings-group-title
   ["|| General ||"
    (transient-macro-call-goto-root)
    (transient-macro-call-magit-todos)
    ]
   ["" ("1" "empty" "empty")]
   ]
  ["|| Specific  ||"  [("2" "empty" "empty")]]
  ["|| SubGroups ||" []]
  ["|| Project   ||" []]
  transient-quit!
  )
)


;;;###autoload
(defun +jg-workspace-run-transient ()
  (interactive)
  (let ((transient--buffer-name jg-workspaces-transient-buffer-name))
    (workspace-control-transient)
    )
  )

;;-- end workspace
