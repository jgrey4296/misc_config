;;; editor/window-control/+bindings.el -*- lexical-binding: t; -*-

;; Bindings
;; (map! :after window-ring-minor-mode
;;       :leader
;;       (:prefix ("w r" . "Ring")
;;         :n "p" #'window-ring-pop-buffer
;;         :n "c" #'window-ring-pop-buffer
;;         :n "C" #'window-ring-clear-ring

;;         :n "l" #'window-ring-move-perspective
;;         :n "h" #'window-ring-move-perspective-2
;;         :n "L" #'window-ring-goto-most-recent
;;         :n "H" #'window-ring-goto-oldest

;;         :n "b" #'window-ring-add-current-buffer
;;         :n "f" #'window-ring-add-to-head
;;         :n "F" #'window-ring-add-to-tail

;;         :n "p" #'window-ring-print-order

;;         :n "s" #'window-ring-setup-columns
;;         :n "r" #'window-ring-remove-buffer

;;         :n "q" #'(lambda () (interactive) (setq window-ring-can-loop (not window-ring-can-loop)))
;;         )
;; )

(after! window-ring-minor-mode
  (progn
    (doom--define-leader-key :states 'normal :infix "w r" "p" #'window-ring-pop-buffer "c" #'window-ring-pop-buffer "C" #'window-ring-clear-ring "l" #'window-ring-move-perspective "h" #'window-ring-move-perspective-2 "L" #'window-ring-goto-most-recent "H" #'window-ring-goto-oldest "b" #'window-ring-add-current-buffer "f" #'window-ring-add-to-head "F" #'window-ring-add-to-tail "p" #'window-ring-print-order "s" #'window-ring-setup-columns "r" #'window-ring-remove-buffer "q"
                             #'(lambda nil
                                 (interactive)
                                 (setq window-ring-can-loop
                                       (not window-ring-can-loop))))
    (doom--define-leader-key :infix "w r" ""
                             (list :ignore t :which-key "Ring"))))
