;;; +ivys.el -*- lexical-binding: t; -*-

(ivy-add-actions '+jg-completion-ivy-workspace
                 '(("r" +jg-workspace-rename "Rename")
                   ("l" +jg-workspace-new-ring "new loop")
                   )
                 )

(defun +jg-workspace-rename (x)
  (+workspace-rename x (read-string (format "Rename %s -> : " x)))
  )

(defun +jg-workspace-new-ring (x)
  (window-ring-new)
  )

(defun +jg-completion-ivy-workspace ()
    "Switch to a workspace or create a new one"
    (interactive)
    (require 'bookmark)
    (ivy-read "Create or jump to workspace: "
              (+workspace-list-names)
              :history 'workspace-history
              :action '+jg-completion-workspace-switch
              :caller '+jg-completion-ivy-workspace)
    )
