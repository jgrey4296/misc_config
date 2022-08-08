;;; +operators.el -*- lexical-binding: t; -*-

(evil-define-operator +jg-fold-wrap-block (beg end type &optional name)
  " Operator to easily create fold blocks "
  :type line
  :keep-visual t
  (interactive "<R>" (list (read-string "Block Name: ")))
  ;; From bottom to top to not change interfere with positions
  ;; add end fold block
  (goto-char end)
  (end-of-line)
  (insert (+jg-fold-block-gen :name name :end t :newlines t))
  ;; and start fold block
  (goto-char beg)
  (beginning-of-line)
  (insert (+jg-fold-block-gen :name name :newlines t))
  )
