;;; util/bindings/+vars.el -*- lexical-binding: t; -*-


(defvar jg-bindings-wk-filter 'id)
(setq   jg-bindings-wk-filter '+jg-bindings-wk-filter-fn)

(setq-default which-key-show-operator-state-maps t
              evil-escape-delay 0.3
              jg-binding-local-var-skip-regexp (rx (or "-map"
                                                       "keymap"
                                                       "display-table"
                                                       "imenu-generic-expression"
                                                       "font-lock-keywords"))

              )


;;-- maps
(setq
      jg-binding-insert-state-map            nil ;; copied in +evil-bindings
      jg-binding-motion-state-map             (make-sparse-keymap "JG map replacing evil-motion-state-map")
      jg-binding-normal-state-map             (make-sparse-keymap "JG map replacing evil-normal-state-map")
      jg-binding-operator-state-map           (make-sparse-keymap "JG map replacing evil-operator-state-map")
      jg-binding-visual-state-map             (make-sparse-keymap "JG map replacing evil-visual-state-map")
      )

;; Submaps
(define-prefix-command 'jg-binding-backward-operator-motion-map nil "jgb-backward-op")
(define-prefix-command 'jg-binding-forward-operator-motion-map  nil "jgb-forward-op")
(define-prefix-command 'jg-binding-backward-general-motion-map  nil "jgb-backward-motion")
(define-prefix-command 'jg-binding-forward-general-motion-map   nil "jgb-forward-motion")
(define-prefix-command 'jg-binding-inner-text-objects-map       nil "jgb-inner")
(define-prefix-command 'jg-binding-outer-text-objects-map       nil "jgb-outer")
(define-prefix-command 'jg-binding-jump-map                     nil "jgb-jump")
(define-prefix-command 'jg-binding-helm-map                     nil "jgb-helm")
(define-prefix-command 'jg-binding-operator-map                 nil "jgb-ops")
(define-prefix-command 'jg-binding-vision-map                   nil "jgb-vision")
(define-prefix-command 'jg-binding-change-map                   nil "jgb-change")
;;-- end maps
