;;; util/bindings/+vars.el -*- lexical-binding: t; -*-


(defvar jg-bindings-wk-filter 'id)
(setq   jg-bindings-wk-filter '+jg-bindings-wk-filter-fn)

(setq-default jg-binding-ibuffer-heuristics (rx (or "backtab"
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

              )


;;-- maps
(setq
      jg-binding-backward-operator-motion-map (make-sparse-keymap "backward motion")
      jg-binding-forward-operator-motion-map  (make-sparse-keymap "forward motion")

      jg-binding-backward-general-motion-map  (make-sparse-keymap "backward motion")
      jg-binding-forward-general-motion-map   (make-sparse-keymap "forward motion")

      jg-binding-inner-text-objects-map       (make-sparse-keymap "JG map for selecting text objects")
      jg-binding-outer-text-objects-map       (make-sparse-keymap "JG map replacing evil-outer-text-objects-map")

      jg-binding-motion-state-map             (make-sparse-keymap "JG map replacing evil-motion-state-map")
      jg-binding-normal-state-map             (make-sparse-keymap "JG map replacing evil-normal-state-map")
      jg-binding-operator-map                 (make-sparse-keymap "JG map of evil operators")
      jg-binding-operator-state-map           (make-sparse-keymap "JG map replacing evil-operator-state-map")
      jg-binding-vision-map                   (make-sparse-keymap "JG vision manipulation")
      jg-binding-visual-state-map             (make-sparse-keymap "JG map replacing evil-visual-state-map")
      jg-binding-insert-state-map       nil ;; copied in +evil-bindings
      jg-binding-help-map               nil ;; copied in +help-bindings
      )
;;-- end maps
