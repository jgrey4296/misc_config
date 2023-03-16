;;; config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+vars")
(load! "+registers")

(after! jg-bindings-total
  (load! "+bindings")
)

(use-package! evil-iedit-state
  :defer t
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (setq iedit-current-symbol-default t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default nil)
  :config
  (define-advice iedit-show-all (:override ()
                                 +jg-misc-iedit-show-all)
    " Override iedit's show all so it doesn't mess with invisible line movement"
    (remove-from-invisibility-spec '(iedit-invisible-overlay-name . t))
    (remove-overlays nil nil iedit-invisible-overlay-name t)
  )

)
(use-package! timeline-mode :defer t)
