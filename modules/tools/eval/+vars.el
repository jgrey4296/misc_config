;;; +vars.el -*- lexical-binding: t; -*-

(defvar +eval-repls nil
  "An alist mapping major modes to plists that describe REPLs. Used by
`+eval/open-repl-other-window' and filled with the `:start' setting.")

(defvar +eval-repl-buffer-name "*repl*")

(defvar +eval-popup-min-lines 4
  "The output height threshold (inclusive) before output is displayed in a popup
buffer rather than an overlay on the line at point or the minibuffer.")

;; remove ellipsis when printing sexps in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)

(setq counsel-compile-root-functions (append counsel-compile-root-functions
                                             `(,#'+jg-eval--compile-root-fallback))
      compilation-always-kill t       ; kill compilation process before starting another
      compilation-ask-about-save nil  ; save all buffers on `compile'
      compilation-scroll-output 'first-error
      )


(speckler-add! popup
                    '(quickrun
                      ("^\\*quickrun" :size 0.3 :ttl 0)
                      )
                    '(compilation
                      ("\\*compilation\\*" :quit t :select nil :height 0.2 :priority 20)
                      )
                    `(repl
                      (,(format "\\%s\\*\\'" +eval-repl-buffer-name) :quit nil :select t :ttl -1 :side right :width 0.3 :priority 50)
                      )
                    )

(speckler-add! compile-commands
                    '(default
                       +jg-workspaces-get-doot-commands
                       ;; counsel-compile-get-filtered-history
                       ;; counsel-compile-get-build-directories
                       ;; counsel-compile-get-make-invocation
                       ;; counsel-compile-get-make-help-invocations
                       )
                    )
