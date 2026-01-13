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

(after! tabulated-list
  ;; Consistently use q to quit windows
  (define-key tabulated-list-mode-map "q" #'quit-window)
  )


(setq esc-map (make-keymap "esc-map")
      lisp-mode-shared-map (make-sparse-keymap "lisp-mode-shared-map")
      ctl-x-map jgb-ctl-x-map
      )

(evil-make-overriding-map messages-buffer-mode-map)
(evil-make-intercept-map read-expression-map)
(keymap-global-set "C-c u" #'universal-argument)

(provide 'jg-global-bindings)
