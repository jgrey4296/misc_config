;;; config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(load! "+state-hl-lines")
(load! "+popup")
(after! hydra   (load! "+hydra"))
(after! core-ui (load! "+advice"))
(after! jg-bindings-total
  (load! "+bindings")
  (after! ibuffer
    (load! "+ibuffer-bindings")
    )
  )

(add-hook 'jg-reapply-hook '+jg-ui-popup-reapply-rules)

(use-package! hl-line
  :defer t
  :init
  (global-hl-line-mode)
  )
(use-package! hi-lock
  :defer t
  :init
  (global-hi-lock-mode)
  :config
  (setq hi-lock-auto-select-face t)
  )
(use-package! auto-highlight-symbol
  :commands auto-highlight-symbol-mode
  :init
  (defvar auto-highlight-symbol-mode nil)
  )
(use-package! whitespace
  :commands whitespace-mode
  :init
  (defvar whitespace-mode nil)
  )
(use-package! centered-cursor-mode
  :commands centered-cursor-mode
  :init
  (defvar centered-cursor-mode nil)
  )
(use-package! evil-visual-mark-mode
  :defer t
  )
(use-package! window-ring-minor-mode
  :commands (window-ring-setup-columns window-ring-minor-mode window-ring-setup-columns-command)
  )
(use-package! palette-mode)
(use-package! paren-state)


;;-- hooks
(add-hook! 'doom-first-file-hook #'+jg-ui-popup-activate-rules)
(add-hook! 'doom-init-ui-hook    #'rainbow-delimiters-mode)
(after! ibuffer
  (add-hook! 'ibuffer-mode-hook    #'+jg-ui-ibuffer-update-hook)
  )

(after! helpful
  (add-hook 'helpful-mode-hook
            (lambda () (set-window-dedicated-p (selected-window) nil)))
  )
;;-- end hooks
