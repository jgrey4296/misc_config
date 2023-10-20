;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-
(doom-log "Setting up Misc Bindings")
(global-set-key (kbd "C-c u") #'universal-argument)

(define-key! read-expression-map
  "C-j" #'next-line-or-history-element
  "C-k" #'previous-line-or-history-element)

(map! :map global-map
      "<f1>" #'ignore
      "<f2>" #'ignore
      "<f3>" #'ignore
      "<f4>" #'ignore
      "<f5>" #'ignore
      "<f6>" #'ignore
      "<f9>" #'ignore
      "<f10>" #'ignore

      )

(map! :map universal-argument-map
      :prefix doom-leader-key     "u" #'universal-argument-more
      :prefix doom-leader-alt-key "u" #'universal-argument-more)

(map! :map special-mode-map
      :n "q" #'quit-window
      )

;; Consistently use q to quit windows
(after! tabulated-list
  (define-key tabulated-list-mode-map "q" #'quit-window))

;;-- evil overrides/intercept
(evil-make-overriding-map messages-buffer-mode-map)
(evil-make-intercept-map read-expression-map)

;;-- end evil overrides/intercept
