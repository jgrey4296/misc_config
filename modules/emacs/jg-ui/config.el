;;; config.el -*- lexical-binding: t; -*-

(load! "+funcs")

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
  ;; Set up faces for hl-line colour sync to status
  (defface jg-evil-normal-state '((t :background  "#000000")) "The Evil Normal State Hl-line")
  (defface jg-evil-insert-state '((t :background  "#005f00")) "The Evil Insert State Hl-line")
  (defface jg-evil-visual-state '((t :background  "#005fff")) "The Evil Visual State Hl-line")
  (defface jg-evil-motion-state '((t :background  "#5f0000")) "The Evil Motion State Hl-line")
  (defface jg-evil-emacs-state '((t :background  "#5f00ff"))  "The Evil Emacs State Hl-line")
  (defface jg-evil-replace-state '((t :background  "#8700ff")) "The Evil Replace State Hl-line")
  (defface jg-evil-hybrid-state '((t :background  "#0087ff")) "The Evil Hybrid State Hl-line")
  (defface jg-evil-evilified-state '((t :background  "#5f5f00")) "The Evil Evilified State Hl-line")
  (defface jg-evil-lisp-state '((t :background  "#875fff")) "The Evil Lisp State Hl-line")
  (defface jg-evil-iedit-state '((t :background  "#8700af")) "The Evil iedit State Hl-line")
  (defface jg-evil-iedit-insert-state '((t :background  "#8700af")) "The Iedit Insert state Hl-line")

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
(after! evil
  (load! "+bindings")
  )
(after! ivy
  (load! "+ivy-actions")
  )

(defun jg-load-popup-hook ()
  (load! "+popup")
  )
(add-hook 'doom-first-input #'jg-load-popup-hook)

;; To overrule ligatures module
(add-hook! 'doom-init-ui-hook :append
  (defun +ligatures-init-h ()
    (remove-hook 'after-change-major-mode-hook #'+ligatures-init-buffer-h)))
