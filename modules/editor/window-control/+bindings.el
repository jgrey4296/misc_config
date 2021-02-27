;;; editor/window-control/+bindings.el -*- lexical-binding: t; -*-

;; Bindings
(map! :leader
      (:prefix ("w r" . "Ring")
        :desc "Pop Buffer (Alt: Pop to here)" :n "c" #'window-ring-pop-buffer
        :desc "Clear Ring" :n "C" #'window-ring-clear-ring

        :desc "Ring Right" :n "l" #'window-ring-move-perspective
        :desc "Ring Left" :n "h" #'window-ring-move-perspective-2
        :desc "Most Recent" :n "L" #'window-ring-goto-most-recent
        :desc "Oldest" :n "H" #'window-ring-goto-oldest

        :desc "Add Current Buffer" :n "b" #'window-ring-add-current-buffer
        :desc "Find File -> Head" :n "f" #'window-ring-add-to-head
        :desc "Find File -> Tail" :n "F" #'window-ring-add-to-tail

        :desc "Print Sequence" :n "p" #'window-ring-print-order

        :desc "Window Ring soft Reset" :n "s" #'+window-ring-block-reset
        :desc "Window Ring Hard Reset" :n "S" #'window-ring-setup-columns-command
        :desc "Remove Current Buffer" :n "R" #'window-ring-remove-buffer
        :desc "Replace with Buffer" :n "r" #'window-ring-replace-buffer

        :desc "Toggle Ring Loop" :n "q" #'(lambda () (interactive) (setq window-ring-can-loop (not window-ring-can-loop)))

        :desc "Shrink Side Wndows" :n "{" #'window-ring-shrink-sides
        )
      )

(map! (:prefix "["
       :desc "Ring Left" :n "r" #'window-ring-move-perspective-2
       )
      (:prefix "]"
       :desc "Ring Right" :n "r" #'window-ring-move-perspective
       )
      )
