;;; util/bindings/+vars.el -*- lexical-binding: t; -*-

(setq-default jg-google-url "https://duckduckgo.com/?q=%s"
              jg-twitter-url "https://twitter.com"

              jg-misc-ibuffer-heuristics (rx (or "backtab"
                                                 (regexp "\\.\\.")
                                                 (regexp "^[[:alpha:]]\\{2,\\}")
                                                 (regexp "which-key")
                                                 (regexp "/ S")
                                                 )
                                             )

              evil-escape-delay 0.3

              jg-binding-local-var-skip-regexp (rx (or "-map"
                                                       "keymap"
                                                       "display-table"
                                                       "imenu-generic-expression"
                                                       "font-lock-keywords"))

              jg-binding-x-in-y-url "https://learnxinyminutes.com"
              )


(setq jg-binding-operator-map           (make-sparse-keymap "evil operators")
      jg-binding-vision-map             (make-sparse-keymap "vision manipulation")
      jg-binding-forward-motion-map     (make-sparse-keymap "forward motion")
      jg-binding-backward-motion-map    (make-sparse-keymap "backward motion")
      jg-binding-inner-text-objects-map (make-sparse-keymap "inner textobjs")
      jg-binding-outer-text-objects-map (make-sparse-keymap "outer textobjs")

      jg-binding-normal-state-map (make-sparse-keymap "jg-binding-normal-state-map")
      jg-binding-visual-state-map (make-sparse-keymap "jg-binding-visual-state-map")
      jg-binding-operator-state-map (make-sparse-keymap "jg-binding-operator-state-map")
      jg-binding-motion-state-map (make-sparse-keymap "jg-binding-motion-state-map")
      )
