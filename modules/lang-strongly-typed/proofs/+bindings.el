;;; +bindings.el -*- lexical-binding: t; -*-

;;-- coq
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

;;-- end coq

;;-- idris
(map! :map idris-mode-map
      :localleader
      "r" #'idris-load-file
      "t" #'idris-type-at-point
      "d" #'idris-add-clause
      "l" #'idris-make-lemma
      "c" #'idris-case-split
      "w" #'idris-make-with-block
      "m" #'idris-add-missing
      "p" #'idris-proof-search
      "h" #'idris-docs-at-point
      )

;;-- end idris

;;-- fstar
(map! :map fstar-mode-map
        :localleader
        :desc "F* next" "]" #'fstar-subp-advance-next
        :desc "F* go to point" "." #'fstar-subp-advance-or-retract-to-point
        :desc "F* previous" "[" #'fstar-subp-retract-last
        (:prefix ("p" . "proof")
          :desc "go to point (lax)" "l" #'fstar-subp-advance-or-retract-to-point-lax
          :desc "compile buffer (lax)" "b" #'fstar-subp-advance-to-point-max-lax
          "q" #'fstar-subp-kill-one-or-many
          "k" #'fstar-subp-kill-z3
          "r" #'fstar-subp-reload-to-point)

        (:prefix ("l" . "layout")
          "c"  #'fstar-quit-windows
          "o"  #'fstar-outline)

        ;; Moving around
        "'" #'fstar-jump-to-related-error
        (:prefix ("j" . "jump")
          "j" #'fstar-jump-to-definition
          "f" #'fstar-jump-to-definition-other-frame
          "w" #'fstar-jump-to-definition-other-window
          "e" #'fstar-jump-to-related-error
          "F" #'fstar-jump-to-related-error-other-frame
          "W" #'fstar-jump-to-related-error-other-window
          "d" #'fstar-visit-dependency
          "a" #'fstar-visit-interface-or-implementation
          :desc "jump to first unprocessed line" "u" #'fstar-subp-goto-beginning-of-unprocessed)

        ;; Help !!!
        (:prefix ("h" . "help")
          "y" #'fstar-copy-help-at-point
          "w" #'fstar-browse-wiki
          "W" #'fstar-browse-wiki-in-browser
          "o" #'fstar-list-options
          "p" #'fstar-quick-peek)

        (:prefix ("a" . "ask (queries)")
          "a" #'fstar-print
          "e" #'fstar-eval
          "E" #'fstar-eval-custom
          "s" #'fstar-search
          "d" #'fstar-doc)

        (:prefix ("i" . "insert")
          "m" #'fstar-insert-match-dwim
          "M" #'fstar-insert-match)
        )

;;-- end fstar

;;-- lean
(map! :map lean-mode-map
      :localleader
      "g" #'lean-toggle-show-goal
      "n" #'lean-toggle-next-error
      (:prefix ("s" . "server")
               "r" #'lean-server-restart
               "s" #'lean-server-stop
               "v" #'lean-server-switch-version)
      (:prefix ("p" . "leanpkg")
               "t" #'lean-leanpkg-test
               "b" #'lean-leanpkg-build
               "c" #'lean-leanpkg-configure)
      "f" #'lean-fill-placeholder
      "h" #'lean-hole
      "m" #'lean-message-boxes-toggle
      "e" #'lean-execute)

;;-- end lean

(after! (coq-mode proof)
  (setq coq-mode-map jg-coq-mode-map
        proof-mode-map jg-coq-proof-mode-map
        )
  )
