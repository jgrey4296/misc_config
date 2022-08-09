;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map coq-mode-map
      :after coq
      "RET"    'proof-goto-point
      "DEL"    'proof-undo-last-successful-command
      "<down>" 'proof-assert-next-command-interactive
      "<up>"   'proof-undo-last-successful-command
      )


(map! :map pasp-mode-map
      :after pasp-mode
      :localleader
      :desc "Prettify" "p" #'prettify-symbols-mode
      )

(evil-make-intercept-map coq-mode-map)

   ;; (spacemacs/set-leader-keys-for-major-mode 'coq-mode
    ;;   (kbd "i m") 'coq-insert-match
    ;;   (kbd "i i") 'coq-insert-intros
    ;;   (kbd "i t t") 'coq-insert-tactic
    ;;   (kbd "i t s") 'coq-insert-solve-tactic
    ;;   )
