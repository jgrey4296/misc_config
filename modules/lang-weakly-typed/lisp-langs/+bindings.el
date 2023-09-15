;;; lang/emacs-lisp/+bindings.el -*- lexical-binding: t; -*-
(doom-log "Setting up lisp bindings")

(setq edebug-eval-mode-map (make-sparse-keymap)
      )
(evil-make-intercept-map edebug-eval-mode-map)

(map! :map emacs-lisp-mode-map
      :n "SPC e" #'eros-eval-last-sexp
      :n "|" #'general-insert-call
      :localleader
      :desc "Sort Defuns" "S"  #'+jg-lisp-sort-defuns
      :desc "Pretty Print" "P" #'pp-buffer
      (:prefix ("e" . "eval")
       :desc "Byte Compile" :n "c" (cmd! (byte-compile-file buffer-file-name))
       )
      (:prefix ("i" . "Insert")
       :desc "Insert Palette Faces" "c" #'+jg-ui-insert-faces
       )
      )

(map! :map (emacs-lisp-mode-map lisp-interaction-mode-map)
      :localleader
      :desc "Expand macro" "m" #'macrostep-expand
      :desc "Collapse Macro" "q" #'macrostep-collapse
      :desc "Collapse All Macros" "Q" #'macrostep-collapse-all
      :desc "EIEIO Browse" "b" #'eieio-browse
      "x" #'doom/sandbox
      (:prefix ("d" . "debug")
               "f" #'+emacs-lisp/edebug-instrument-defun-on
               "F" #'+emacs-lisp/edebug-instrument-defun-off)
      (:prefix ("e" . "eval")
               "b" #'eval-buffer
               "d" #'eval-defun
               "e" #'eval-last-sexp
               "r" #'eval-region
               "l" #'load-library)
      (:prefix ("g" . "goto")
               "f" #'find-function
               "v" #'find-variable
               "l" #'find-library)
      )

(map! :map racket-xp-mode-map
      [remap racket-doc]              #'racket-xp-documentation
      [remap racket-visit-definition] #'racket-xp-visit-definition
      [remap next-error]              #'racket-xp-next-error
      [remap previous-error]          #'racket-xp-previous-error
      )

(map! :map racket-mode-map
      :localleader
      "a" #'racket-align
      "A" #'racket-unalign
      "f" #'racket-fold-all-tests
      "F" #'racket-unfold-all-tests
      "h" #'racket-doc
      "i" #'racket-unicode-input-method-enable
      "l" #'racket-logger
      "o" #'racket-profile
      "p" #'racket-cycle-paren-shapes
      "r" #'racket-run
      "R" #'racket-run-and-switch-to-repl
      "t" #'racket-test
      "u" #'racket-backward-up-list
      "y" #'racket-insert-lambda
      (:prefix ("m" . "macros")
               "d" #'racket-expand-definition
               "e" #'racket-expand-last-sexp
               "r" #'racket-expand-region
               "a" #'racket-expand-again)
      (:prefix ("g" . "goto")
               "b" #'racket-unvisit
               "d" #'racket-visit-definition
               "m" #'racket-visit-module
               "r" #'racket-open-require-path)
      (:prefix ("s" . "send")
               "d" #'racket-send-definition
               "e" #'racket-send-last-sexp
               "r" #'racket-send-region)
      )

(map! :map racket-repl-mode-map
      "l" #'racket-logger
      "h" #'racket-repl-documentation
      "y" #'racket-insert-lambda
      "u" #'racket-backward-up-list
      (:prefix ("m" . "macros")
               "d" #'racket-expand-definition
               "e" #'racket-expand-last-sexp
               "f" #'racket-expand-file
               "r" #'racket-expand-region)
      (:prefix ("g" . "goto")
               "b" #'racket-unvisit
               "m" #'racket-visit-module
               "d" #'racket-repl-visit-definition)
      )

(map! :map edebug-eval-mode-map
      ;; TODO add a reminder for this
      "s" #'edebug-step-mode
      "q" #'edebug-stop
      "n" #'edebug-next-mode
      "t" #'edebug-trace-mode
      "d" #'edebug-trace-display
      "i" #'edebug-step-in
      "o" #'edebug-step-out
      "g" #'edebug-go-mode
      )

(map! :map buttercup-minor-mode-map
      :localleader
      :prefix "t"
      "t" #'+emacs-lisp/buttercup-run-file
      "a" #'+emacs-lisp/buttercup-run-project
      "s" #'buttercup-run-at-point
      )


(map! :map dired-mode-map
      :after jg-dired-bindings
      :prefix ("> l" . "lisp")
      :desc "byte compile"        "c" #'dired-do-byte-compile
      )
