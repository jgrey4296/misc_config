;;; util/bindings/+vars.el -*- lexical-binding: t; -*-


(defvar jg-bindings-wk-filter 'id)
(setq jg-bindings-wk-filter '+jg-bindings-wk-filter-fn)

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
              jg-binding-debug-snippet-name "util.debug"
              )


(setq jg-binding-backward-motion-map    (make-sparse-keymap "backward motion")
      jg-binding-forward-motion-map     (make-sparse-keymap "forward motion")
      jg-binding-inner-text-objects-map (make-sparse-keymap "JG map for selectin text objects")
      jg-binding-insert-state-map       nil ;; copied in +evil-bindings
      jg-binding-motion-state-map       (make-sparse-keymap "JG map replacing evil-motion-state-map")
      jg-binding-normal-state-map       (make-sparse-keymap "JG map replacing evil-normal-state-map")
      jg-binding-operator-map           (make-sparse-keymap "evil operators")
      jg-binding-operator-state-map     (make-sparse-keymap "JG map replacing evil-operator-state-map")
      jg-binding-outer-text-objects-map (make-sparse-keymap "JG map replacing evil-outer-text-objects-map")
      jg-binding-vision-map             (make-sparse-keymap "vision manipulation")
      jg-binding-visual-state-map       (make-sparse-keymap "JG map replacing evil-visual-state-map")
      )
