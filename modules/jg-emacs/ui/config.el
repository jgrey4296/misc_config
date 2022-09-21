;;; config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(load! "+faces")
(after! hydra
  (load! "+hydra")
  )
(after! core-ui
  (load! "+advice")
  )
(after! evil
  (load! "+bindings")
  )
(load! "+popup")

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

;;-- hooks
(add-hook! 'doom-first-file-hook #'+jg-ui-popup-activate-rules)

(after! (evil hl-line)
  ;; hooks for evil state entry hooks to change hl-line colour
  ;; (add-hook 'evil-normal-state-entry-hook       (-partial #'+jg-ui-hl-line-colour 'jg-evil-normal-state-face))
  ;; (add-hook 'evil-insert-state-entry-hook       (-partial #'+jg-ui-hl-line-colour 'jg-evil-insert-state-face))
  ;; (add-hook 'evil-visual-state-entry-hook       (-partial #'+jg-ui-hl-line-colour 'jg-evil-visual-state-face))
  ;; (add-hook 'evil-motion-state-entry-hook       (-partial #'+jg-ui-hl-line-colour 'jg-evil-motion-state-face))
  ;; (add-hook 'evil-emacs-state-entry-hook        (-partial #'+jg-ui-hl-line-colour 'jg-evil-emacs-state-face))
  ;; (add-hook 'evil-replace-state-entry-hook      (-partial #'+jg-ui-hl-line-colour 'jg-evil-replace-state-face))
  ;; (add-hook 'evil-hybrid-state-entry-hook       (-partial #'+jg-ui-hl-line-colour 'jg-evil-hybrid-state-face))
  ;; (add-hook 'evil-evilified-state-entry-hook    (-partial #'+jg-ui-hl-line-colour 'jg-evil-evilified-state-face))
  ;; (add-hook 'evil-lisp-state-entry-hook         (-partial #'+jg-ui-hl-line-colour 'jg-evil-lisp-state-face))
  ;; (add-hook 'evil-iedit-state-entry-hook        (-partial #'+jg-ui-hl-line-colour 'jg-evil-iedit-state-face))
  ;; (add-hook 'evil-iedit-insert-state-entry-hook (-partial #'+jg-ui-hl-line-colour 'jg-evil-iedit-insert-state-face))
  )
(after! helpful
  (add-hook 'helpful-mode-hook
            (lambda () (set-window-dedicated-p (selected-window) nil)))
  )
;;-- end hooks
