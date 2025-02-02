;;; editor/fold/config.el -*- lexical-binding: t; -*-

;;
;; Packages
(local-load! "+vars")
(local-load! "+spec-defs")
(defer-load! jg-bindings-total "+bindings")

(defconst fold-modes '(vimish-fold-mode
                       hs-minor-mode
                       origami-mode
                       outline-minor-mode
                       hide-ifdef-mode
                       )
  )

(use-package! hideshow ; built-in
  :commands (hs-toggle-hiding hs-hide-block hs-hide-level hs-show-all hs-hide-all)
  :config
  (advice-add 'hs-toggle-hiding :before #'+fold--hideshow-ensure-mode-a)
  (advice-add 'hs-hide-block :before #'+fold--hideshow-ensure-mode-a)
  (advice-add 'hs-hide-level :before #'+fold--hideshow-ensure-mode-a)
  (advice-add 'hs-show-all :before #'+fold--hideshow-ensure-mode-a)
  (advice-add 'hs-hide-all :before #'+fold--hideshow-ensure-mode-a)
  )

(use-package! vimish-fold)

(use-package! origami :defer t)

(use-package! code-shy-minor-mode
  :init
  (setq code-shy-fold-patterns (list "%s-- %s %s" "%s-- %s %s"))

  (add-hook! doom-first-buffer
             #'global-code-shy-minor-mode
             )
  )

(use-package! outline)
