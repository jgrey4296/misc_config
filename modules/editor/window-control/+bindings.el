;;; editor/window-control/+bindings.el -*- lexical-binding: t; -*-

;; Bindings
;; (map! :after window-ring-minor-mode
;;       :leader
;;       (:prefix ("w r" . "Ring")
;;         :n :desc "Pop Buffer (Alt: Pop to here)" "c" #'window-ring-pop-buffer
;;         :n :desc "Clear Ring" "C" #'window-ring-clear-ring

;;         :n :desc "Ring Right" "l" #'window-ring-move-perspective
;;         :n :desc "Ring Left" "h" #'window-ring-move-perspective-2
;;         :n :desc "Most Recent" "L" #'window-ring-goto-most-recent
;;         :n :desc "Oldest" "H" #'window-ring-goto-oldest

;;         :n :desc "Add Current Buffer" "b" #'window-ring-add-current-buffer
;;         :n :desc "Find File -> Head" "f" #'window-ring-add-to-head
;;         :n :desc "Find File -> Tail" "F" #'window-ring-add-to-tail

;;         :n :desc "Print Sequence" "p" #'window-ring-print-order

;;         :n :desc "Window Ring soft Reset" "s" #'+window-ring-block-reset
;;         :n :desc "Window Ring Hard Reset" "S" #'window-ring-setup-columns-command
;;         :n :desc "Remove Current Buffer""r" #'window-ring-remove-buffer

;;         :n :desc "Toggle Ring Loop" "q" #'(lambda () (interactive) (setq window-ring-can-loop (not window-ring-can-loop)))

;;         :n "{" #'window-ring-shrink-sides
;;         )
;;       )

(after! window-ring-minor-mode
  (progn
    (doom--define-leader-key :states 'normal :infix "w r" :desc "Pop Buffer (Alt: Pop to here)" :desc "Clear Ring" :desc "Ring Right" :desc "Ring Left" :desc "Most Recent" :desc "Oldest" :desc "Add Current Buffer" :desc "Find File -> Head" :desc "Find File -> Tail" :desc "Print Sequence" :desc "Window Ring soft Reset" :desc "Window Ring Hard Reset" :desc "Remove Current Buffer" :desc "Toggle Ring Loop" "{" #'window-ring-shrink-sides)
    (doom--define-leader-key :infix "w r" ""
                             (list :ignore t :which-key "Ring")
                             "c" #'window-ring-pop-buffer "C" #'window-ring-clear-ring "l" #'window-ring-move-perspective "h" #'window-ring-move-perspective-2 "L" #'window-ring-goto-most-recent "H" #'window-ring-goto-oldest "b" #'window-ring-add-current-buffer "f" #'window-ring-add-to-head "F" #'window-ring-add-to-tail "p" #'window-ring-print-order "s" #'+window-ring-block-reset "S" #'window-ring-setup-columns-command "r" #'window-ring-remove-buffer "q"
                             #'(lambda nil
                                 (interactive)
                                 (setq window-ring-can-loop
                                       (not window-ring-can-loop))))))
