;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-
(dlog! "Setting up Misc Bindings")

(map! :map read-expression-map
  "C-j" #'next-line-or-history-element
  "C-k" #'previous-line-or-history-element
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

(setq esc-map (make-keymap)
      lisp-mode-shared-map (make-sparse-keymap)
      ctl-x-map jg-ctl-x-map
      )
;; (use-global-map global-map)

(keymap-global-set "C-c u" #'universal-argument)
(provide 'jg-global-bindings)
