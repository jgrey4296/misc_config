;;; editor/fold/config.el -*- lexical-binding: t; -*-

;;
;; Packages
(load! "+vars")
(after! jg-bindings-total
  (load! "+bindings")
  )

(use-package! hideshow ; built-in
  :commands (hs-toggle-hiding hs-hide-block hs-hide-level hs-show-all hs-hide-all)
  :config
  (defadvice! +fold--hideshow-ensure-mode-a (&rest _)
    "Ensure `hs-minor-mode' is enabled when we need it, no sooner or later."
    :before '(hs-toggle-hiding hs-hide-block hs-hide-level hs-show-all hs-hide-all)
    (unless (bound-and-true-p hs-minor-mode)
      (hs-minor-mode +1)))
  )

(use-package! evil-vimish-fold
  :commands (evil-vimish-fold/next-fold evil-vimish-fold/previous-fold evil-vimish-fold/delete evil-vimish-fold/delete-all evil-vimish-fold/create evil-vimish-fold/create-line)
  :init
  (global-evil-vimish-fold-mode)
  )

(use-package! origami :defer t)

(use-package! autohide-minor-mode)

(add-hook! 'jg-ui-reapply-hook
           #'+jg-fold-reapply-specs
           #'+jg-fold-hideshow-reapply-specs
           )
