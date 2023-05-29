;;; +bindings.el -*- lexical-binding: t; -*-

(defvar jg-coq-mode-map (make-sparse-keymap))
(evil-make-intercept-map jg-coq-mode-map)

(map! :map jg-coq-mode-map
      :localleader
      "]"  #'proof-assert-next-command-interactive
      "["  #'proof-undo-last-successful-command
      "."  #'proof-goto-point
      (:prefix ("l" . "layout")
        "c" #'pg-response-clear-displays
        "l" #'proof-layout-windows
        "p" #'proof-prf)
      (:prefix ("p" . "proof")
        "i" #'proof-interrupt-process
        "p" #'proof-process-buffer
        "q" #'proof-shell-exit
        "r" #'proof-retract-buffer)
      (:prefix ("a" . "about/print/check")
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
          "i" #'coq-Print-with-implicits))
      (:prefix ("g" . "goto")
        "e" #'proof-goto-command-end
        "l" #'proof-goto-end-of-locked
        "s" #'proof-goto-command-start)
      (:prefix ("i" . "insert")
        "c" #'coq-insert-command
        "e" #'coq-end-Section
        "i" #'coq-insert-intros
        "r" #'coq-insert-requires
        "s" #'coq-insert-section-or-module
        "t" #'coq-insert-tactic
        "T" #'coq-insert-tactical)
      )
(map! :map jg-coq-mode-map
        :localleader
        "ao" #'company-coq-occur
        (:prefix "i"
          "l" #'company-coq-lemma-from-goal
          "m" #'company-coq-insert-match-construct)
        (:prefix ("h" . "help")
          "e" #'company-coq-document-error
          "E" #'company-coq-browse-error-messages
          "h" #'company-coq-doc))

(map! :map jg-coq-mode-map
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


(after! coq-mode
  (setq coq-mode-map jg-coq-mode-map)
  )
