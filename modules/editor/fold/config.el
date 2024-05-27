;;; editor/fold/config.el -*- lexical-binding: t; -*-

;;
;; Packages
(local-load! "+vars")
(local-load! "+spec-defs")
(defer-load! jg-bindings-total "+bindings")

(use-package! hideshow ; built-in
  :commands (hs-toggle-hiding hs-hide-block hs-hide-level hs-show-all hs-hide-all)
  :config
  (advice-add 'hs-toggle-hiding :before #'+fold--hideshow-ensure-mode-a)
  (advice-add 'hs-hide-block :before #'+fold--hideshow-ensure-mode-a)
  (advice-add 'hs-hide-level :before #'+fold--hideshow-ensure-mode-a)
  (advice-add 'hs-show-all :before #'+fold--hideshow-ensure-mode-a)
  (advice-add 'hs-hide-all :before #'+fold--hideshow-ensure-mode-a)
  )

(use-package! evil-vimish-fold
  :commands (evil-vimish-fold/next-fold evil-vimish-fold/previous-fold evil-vimish-fold/delete evil-vimish-fold/delete-all evil-vimish-fold/create evil-vimish-fold/create-line)
  :init
  (global-evil-vimish-fold-mode)
  )

(use-package! origami :defer t)

(use-package! code-shy-minor-mode
  :init
  (setq code-shy-fold-patterns (list "%s-- %s %s" "%s-- %s %s"))

  (add-hook! doom-first-buffer
             #'global-code-shy-minor-mode
             )
  )
