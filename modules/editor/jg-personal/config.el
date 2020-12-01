;; jg_emacs config.el
;; loaded fourth

(load! "+variables")
(load! "+bindings")
(load! "+funcs")
(load! "+evil-ex-setup")
(load! "+helm-funcs")
(load! "+ibuffer-funcs")
(load! "+dired-funcs")


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
(use-package! semantic
  :defer t
  :config (add-to-list 'semantic-default-submodes
                       'global-semantic-idle-summary-mode))
(use-package! evil-iedit-state
  :defer t
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init
  (setq iedit-current-symbol-default t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default nil)
  :config
  (defun iedit-show-all()
    """ Override iedit's show all so it doesn't mess with invisible line movement"
    (remove-from-invisibility-spec '(iedit-invisible-overlay-name . t))
    (remove-overlays nil nil iedit-invisible-overlay-name t)
  )
)

(after! (evil evil-snipe)
  (push 'dired-mode evil-snipe-disabled-modes)

  )
(after! yasnippet
  ;; If an error occurs, change yas-installed-snippets-dir is not in yas-snippet-dirs
  ;; as it is obsolete
  (setq yas-snippet-dirs `(,(expand-file-name "~/.doom.d/snippets/")))
  (setq yas--default-user-snippets-dir yas-snippet-dirs)
  ;; ,(expand-file-name "~/github/otherLibs/yasnippet-snippets/snippets")
  ;; ,(expand-file-name "~/github/otherLibs/yasnippet-snippets")
  )
(after! evil-quickscope
  ;; TODO (spacemacs/set-leader-keys "t q" '+jg-personal-toggle-quickscope-always)
  (global-evil-quickscope-always-mode 1)
  )
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
(after! (dired dired-quick-sort)
  (setq dired-quick-sort-group-directories-last ?y)
  )
(after! ibuffer
  (add-transient-hook! 'ibuffer-hook '+jg-personal-setup-ibuffer)
  (push 'ibuffer-mode evil-snipe-disabled-modes)
  )
(after! neotree
  (push "^__pycache__$" neo-hidden-regexp-list)
  (push "^G\\(PATH\\|R?TAGS\\)$" neo-hidden-regexp-list)
  (push "^__init__.py$" neo-hidden-regexp-list)
  )
(use-package! helm-gtags
  :defer t)


(after! (featurep! :completion helm)
    (setq! helm-find-files-actions
          (append `(,(car helm-find-files-actions))
                  '(("Open Random" . +jg-personal-helm-open-random-action))
                  '(("Describe Random" . +jg-personal-helm-describe-random-action))
                  '(("Open Random External" . +jg-personal-helm-open-random-external-action))
                  (cdr helm-find-files-actions))
          )
    )
