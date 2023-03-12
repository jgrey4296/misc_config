;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map coq-mode-map
      :after coq
      "RET" nil
      "DEL" nil
      :n "RET"    'proof-goto-point
      :n "DEL"    'proof-undo-last-successful-command
      :n "<down>" 'proof-assert-next-command-interactive
      :n "<up>"   'proof-undo-last-successful-command

      :i "RET"    'newline
      :i "DEL"    'backward-delete-char-untabify
      )

(map! :map proof-mode-map
      :after proof
      "RET" nil
      "DEL" nil
      :n "RET"    'proof-goto-point
      :n "DEL"    'proof-undo-last-successful-command
      :n "<down>" 'proof-assert-next-command-interactive
      :n "<up>"   'proof-undo-last-successful-command

      :i "RET"    'newline
      :i "DEL"    'backward-delete-char-untabify

      )

(map! :map pasp-mode-map
      :after pasp-mode
      :localleader
      :desc "Prettify" "p" #'prettify-symbols-mode
      :desc "Compile"  "c" #'pasp-run-buffer
      )

(map! :map jacamo-mode-map
      :localleader
      :desc "Jacamo Github" :n "1" (cmd! (browse-url "https://github.com/jacamo-lang/jacamo"))
      :desc "Jacamo API" :n "3" (cmd! (browse-url "https://jacamo.sourceforge.net/doc/api/index.html"))
      :desc "Jason API"  :n "4" (cmd! (browse-url "https://jason.sourceforge.net/api/index.html"))

      )

(evil-make-intercept-map pasp-mode-map)
(evil-make-intercept-map coq-mode-map)

   ;; (spacemacs/set-leader-keys-for-major-mode 'coq-mode
    ;;   (kbd "i m") 'coq-insert-match
    ;;   (kbd "i i") 'coq-insert-intros
    ;;   (kbd "i t t") 'coq-insert-tactic
    ;;   (kbd "i t s") 'coq-insert-solve-tactic
    ;;   )
