;;; config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(load! "utils/+state-hl-lines")
(load! "utils/+hooks")
(load!  "popup/+funcs")
(load! "ibuffer/+funcs")

(after! module/popup
    (load! "popup/config.el")
    )
(after! hydra (load! "hydra/+hydra"))
(after! jg-bindings-total
  (load! "+bindings")
  )

(after! (jg-bindings-total ibuffer)
  (load! "ibuffer/+ibuffer-bindings")
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
(use-package! evil-visual-mark-mode
  :defer t
  )
(use-package! window-ring-minor-mode
  :commands (window-ring-setup-columns window-ring-minor-mode window-ring-setup-columns-command)
  )
(use-package! palette-mode :defer t)
(use-package! paren-state :defer t)
