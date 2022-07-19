;;; +bindings.el -*- lexical-binding: t; -*-

(map! :after coq
      :map coq-mode-map
      "RET"    'proof-goto-point
      "DEL"    'proof-undo-last-successful-command
      "<down>" 'proof-assert-next-command-interactive
      "<up>"   'proof-undo-last-successful-command
      )

   ;; (spacemacs/set-leader-keys-for-major-mode 'coq-mode
    ;;   (kbd "i m") 'coq-insert-match
    ;;   (kbd "i i") 'coq-insert-intros
    ;;   (kbd "i t t") 'coq-insert-tactic
    ;;   (kbd "i t s") 'coq-insert-solve-tactic
    ;;   )
