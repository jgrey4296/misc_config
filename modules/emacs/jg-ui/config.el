;;; config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+faces")
(after! evil
  (load! "+bindings")
  )
(after! ivy
  (load! "+ivy-actions")
  )
(after! popup
  (load! "+popup")
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
  :defer t
  )
(use-package! centered-cursor-mode
  :defer t
  :commands centered-cursor-mode
  )
(use-package! evil-visual-mark-mode
  :defer t
  )
(use-package! window-ring-minor-mode
  :commands (window-ring-setup-columns window-ring-minor-mode window-ring-setup-columns-command)
  )
(use-package! palette-mode)

(after! (evil hl-line)
  ;; hooks for evil state entry hooks to change hl-line colour
  (add-hook 'evil-normal-state-entry-hook       #'(lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'jg-evil-normal-state))))
  (add-hook 'evil-insert-state-entry-hook       #'(lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'jg-evil-insert-state))))
  (add-hook 'evil-visual-state-entry-hook       #'(lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'jg-evil-visual-state))))
  (add-hook 'evil-motion-state-entry-hook       #'(lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'jg-evil-motion-state))))
  (add-hook 'evil-emacs-state-entry-hook        #'(lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'jg-evil-emacs-state))))
  (add-hook 'evil-replace-state-entry-hook      #'(lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'jg-evil-replace-state))))
  (add-hook 'evil-hybrid-state-entry-hook       #'(lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'jg-evil-hybrid-state))))
  (add-hook 'evil-evilified-state-entry-hook    #'(lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'jg-evil-evilified-state))))
  (add-hook 'evil-lisp-state-entry-hook         #'(lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'jg-evil-lisp-state))))
  (add-hook 'evil-iedit-state-entry-hook        #'(lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'jg-evil-iedit-state))))
  (add-hook 'evil-iedit-insert-state-entry-hook #'(lambda () (interactive) (if (overlayp global-hl-line-overlay) (overlay-put global-hl-line-overlay 'face 'jg-evil-iedit-insert-state))))
  )
(after! (featurep! :completion helm)
    (setq! helm-find-files-actions
          (append `(,(car helm-find-files-actions))
                  '(("Open Random" . +jg-personal-helm-open-random-action))
                  '(("Describe Random" . +jg-personal-helm-describe-random-action))
                  '(("Open Random External" . +jg-personal-helm-open-random-external-action))
                  (cdr helm-find-files-actions))
          )
    )

;; To overrule ligatures module
(add-hook! 'doom-init-ui-hook :append
  (defun +ligatures-init-h ()
    (remove-hook 'after-change-major-mode-hook #'+ligatures-init-buffer-h)))
