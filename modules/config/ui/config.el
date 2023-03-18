;;; config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(load! "utils/+state-hl-lines")
(load! "utils/+faces")
(load! "utils/+narrowing")
(after! (evil jg-bindings-total)
  (load! "+bindings")
  )

(add-hook! 'doom-init-ui-hook  'rainbow-delimiters-mode)
(add-hook! 'doom-init-ui-hook (defun +jg-ui-load-advice ()
                                (load! "utils/+advice")
                                )
           )

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
(use-package! window-ring-minor-mode
  :commands (window-ring-setup-columns window-ring-minor-mode window-ring-setup-columns-command)
  )
(use-package! palette-mode :defer t)
(use-package! evil-visual-mark-mode :defer t)
