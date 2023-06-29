;;; +bindings.el -*- lexical-binding: t; -*-

(defvar jg-coq-mode-map (make-sparse-keymap))

(defvar jg-coq-proof-mode-map (make-sparse-keymap))
;; (evil-make-intercept-map jg-coq-mode-map)
;; (evil-make-intercept-map jg-coq-proof-mode-map)

(map! :map jg-coq-mode-map
      :n "]"  #'proof-assert-next-command-interactive
      :n "["  #'proof-undo-last-successful-command
      :n "."  #'proof-goto-point

      :n "RET"    #'proof-goto-point
      :n "DEL"    #'proof-undo-last-successful-command
      :n "<down>" #'proof-assert-next-command-interactive
      :n "<up>"   #'proof-undo-last-successful-command

      :i "RET"    #'newline
      :i "DEL"    #'backward-delete-char-untabify

      :n "so" #'company-coq-occur
      )

(map! :map jg-coq-mode-map ;; layout
      :localleader
      :prefix ("l" . "layout")
      "c" #'pg-response-clear-displays
      "l" #'proof-layout-windows
      "p" #'proof-prf
      )

(map! :map jg-coq-mode-map ;; proofs
      :localleader
      :prefix ("p" . "proof")
      "i" #'proof-interrupt-process
      "p" #'proof-process-buffer
      "q" #'proof-shell-exit
      "r" #'proof-retract-buffer
      )

(map! :map jg-coq-mode-map ;; print
      :localleader
      :prefix ("a" . "about/print/check")
      "a" #'coq-Print
      "A" #'coq-Print-with-all
      "b" #'coq-About
      "B" #'coq-About-with-all
      "c" #'coq-Check
      "C" #'coq-Check-show-all
      "f" #'proof-find-theorems
      (:prefix ("i" . "implicits")
               "b" #'coq-About-with-implicits
               "c" #'coq-Check-show-implicits
               "i" #'coq-Print-with-implicits)

      )

(map! :map jg-coq-mode-map ;; goto
      :localleader
      :prefix ("g" . "goto")
      "e" #'proof-goto-command-end
      "l" #'proof-goto-end-of-locked
      "s" #'proof-goto-command-start
      )

(map! :map jg-coq-mode-map ;; insert
      :localleader
      :prefix ("i" . "insert")
      "c" #'coq-insert-command
      "e" #'coq-end-Section
      "i" #'coq-insert-intros
      "r" #'coq-insert-requires
      "s" #'coq-insert-section-or-module
      "t" #'coq-insert-tactic
      "T" #'coq-insert-tactical
      "l" #'company-coq-lemma-from-goal
      "m" #'company-coq-insert-match-construct
      )

(map! :map jg-coq-mode-map ;; help
      :localleader
      :prefix ("h" . "help")
      "e" #'company-coq-document-error
      "E" #'company-coq-browse-error-messages
      "h" #'company-coq-doc
      )

(after! (coq-mode proof)
  (setq coq-mode-map jg-coq-mode-map
        proof-mode-map jg-coq-proof-mode-map
        )
  )
