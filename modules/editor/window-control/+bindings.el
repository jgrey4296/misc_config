;;; editor/window-control/+bindings.el -*- lexical-binding: t; -*-

;; Bindings
(map! :leader
      (:prefix ("w r" . "Ring")
        :n :desc "Pop Buffer (Alt: Pop to here)" "c" #'window-ring-pop-buffer
        :n :desc "Clear Ring" "C" #'window-ring-clear-ring

        :n :desc "Ring Right" "l" #'window-ring-move-perspective
        :n :desc "Ring Left" "h" #'window-ring-move-perspective-2
        :n :desc "Most Recent" "L" #'window-ring-goto-most-recent
        :n :desc "Oldest" "H" #'window-ring-goto-oldest

        :n :desc "Add Current Buffer" "b" #'window-ring-add-current-buffer
        :n :desc "Find File -> Head" "f" #'window-ring-add-to-head
        :n :desc "Find File -> Tail" "F" #'window-ring-add-to-tail

        :n :desc "Print Sequence" "p" #'window-ring-print-order

        :n :desc "Window Ring soft Reset" "s" #'+window-ring-block-reset
        :n :desc "Window Ring Hard Reset" "S" #'window-ring-setup-columns-command
        :n :desc "Remove Current Buffer""r" #'window-ring-remove-buffer

        :n :desc "Toggle Ring Loop" "q" #'(lambda () (interactive) (setq window-ring-can-loop (not window-ring-can-loop)))

        :n :desc "Shrink Side Wndows" "{" #'window-ring-shrink-sides
        )
      )

