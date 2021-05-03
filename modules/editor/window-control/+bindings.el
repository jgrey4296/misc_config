;;; editor/window-control/+bindings.el -*- lexical-binding: t; -*-

;; Bindings
(map! :leader
      :prefix ("w r" . "Ring")
      :desc "Pop Buffer (Alt: Pop to here)"  "c" #'window-ring-pop-buffer
      :desc "Clear Ring"                     "C" #'window-ring-clear-ring

      :desc "Ring Right"                     "l" #'window-ring-move-perspective
      :desc "Ring Left"                      "h" #'window-ring-move-perspective-2
      :desc "Most Recent"                    "L" #'window-ring-goto-most-recent
      :desc "Oldest"                         "H" #'window-ring-goto-oldest

      :desc "Add Current Buffer"             "b" #'window-ring-add-current-buffer
      :desc "Find File -> Head"              "f" #'window-ring-add-to-head
      :desc "Find File -> Tail"              "F" #'window-ring-add-to-tail

      :desc "Print Sequence"                 "p" #'window-ring-print-order

      :desc "Window Ring soft Reset"         "s" #'+window-ring-block-reset
      :desc "Window Ring Hard Reset"         "S" #'window-ring-setup-columns-command
      :desc "Remove Current Buffer"          "R" #'window-ring-remove-buffer
      :desc "Replace with Buffer"            "r" #'window-ring-replace-buffer

      :desc "Toggle Ring Loop"               "q" #'(lambda () (interactive) (setq window-ring-can-loop (not window-ring-can-loop)))

      :desc "Shrink Side Wndows"             "{" #'window-ring-shrink-sides
      )
