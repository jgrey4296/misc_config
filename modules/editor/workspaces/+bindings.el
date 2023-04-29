;;; +bindings.el -*- lexical-binding: t; -*-

;; Delete the current workspace if closing the last open window
(define-key! persp-mode-map
  [remap delete-window] #'+workspace/close-window-or-workspace
  [remap evil-window-delete] #'+workspace/close-window-or-workspace)


;;-- window ring
(map! :leader
      :prefix ("W" . "Window Ring")
      :desc "New Ring"                       "n" #'window-ring-new
      :desc "Clear Ring"                     "c" #'window-ring-clear-ring
      :desc "Edit Ring"                      "e" #'window-ring-edit-order
      :desc "Print Sequence"                 "p" #'window-ring-print-order
      :desc "Window Ring Hard Reset"         "r" #'window-ring-reset-columns
      :desc "Toggle Ring Loop"               "l" #'window-ring-toggle-loop
      :desc "Shrink Side Wndows"             "s" #'window-ring-shrink-sides
      )

(map! :leader
      :prefix "w"
      ;; :desc "Ring Right"                     "l" #'window-ring-move-focus
      ;; :desc "Ring Left"                      "h" #'window-ring-move-focus-alt
      :desc "Most Recent"                    "L" #'window-ring-goto-most-recent
      :desc "Oldest"                         "H" #'window-ring-goto-oldest
      :desc "Print Ring"                     "p" #'window-ring-print-order
      )

;;-- end window ring


(map! :map window-ring-edit-map
      "C-c C-c" #'window-ring-edit-commit)

(map! :map jg-binding-backward-general-motion-map
      :desc "Ring Window"  "r"    #'window-ring-move-focus-alt
      :desc "Workspace"    "w"    #'+workspace/switch-left
      )

(map! :map jg-binding-forward-general-motion-map
      :desc "Ring Window"  "r"    #'window-ring-move-focus
      :desc "Workspace"    "w"    #'+workspace/switch-right
)
