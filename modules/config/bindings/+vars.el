;;; util/bindings/+vars.el -*- lexical-binding: t; -*-


(defvar jg-bindings-wk-filter '+jg-bindings-wk-filter-fn)

(setq-default which-key-show-operator-state-maps t)


(setq which-key-sort-order #'which-key-key-order-alpha
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1
      which-key-max-display-columns nil
      which-key-min-display-lines 6
      which-key-side-window-slot -10

      which-key-idle-secondary-delay 0.05
      )
