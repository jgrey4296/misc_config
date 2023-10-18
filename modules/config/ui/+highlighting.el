;; +higlighting.el -*- mode: elisp; lexical-binding: t; -*-


(use-package! hl-line
  :defer t
  :hook (doom-first-file . global-hl-line-mode)
  :config
  (after! evil
    ;; hooks for evil state entry hooks to change hl-line colour
    (add-hook! '(evil-normal-state-entry-hook
                 evil-insert-state-entry-hook
                 evil-visual-state-entry-hook
                 evil-motion-state-entry-hook
                 evil-emacs-state-entry-hook
                 evil-replace-state-entry-hook
                 evil-hybrid-state-entry-hook
                 evil-evilified-state-entry-hook
                 evil-lisp-state-entry-hook
                 evil-iedit-state-entry-hook)
               #'+jg-ui-state-line-change)
    )

  ;; HACK I reimplement `global-hl-line-mode' so we can white/blacklist modes in
  ;;      `global-hl-line-modes' _and_ so we can use `global-hl-line-mode',
  ;;      which users expect to control hl-line in Emacs.
  (define-globalized-minor-mode global-hl-line-mode hl-line-mode
    (lambda ()
      (and (cond (hl-line-mode nil)
                 ((null global-hl-line-modes) nil)
                 ((eq global-hl-line-modes t))
                 ((eq (car global-hl-line-modes) 'not)
                  (not (derived-mode-p global-hl-line-modes)))
                 ((apply #'derived-mode-p global-hl-line-modes)))
           (hl-line-mode +1))))

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar doom--hl-line-mode nil)

  (add-hook! 'hl-line-mode-hook
    (defun doom-truly-disable-hl-line-h ()
      (unless hl-line-mode
        (setq-local doom--hl-line-mode nil))))

  (add-hook! '(evil-visual-state-entry-hook activate-mark-hook)
    (defun doom-disable-hl-line-h ()
      (when hl-line-mode
        (hl-line-mode -1)
        (setq-local doom--hl-line-mode t))))

  (add-hook! '(evil-visual-state-exit-hook deactivate-mark-hook)
    (defun doom-enable-hl-line-maybe-h ()
      (when doom--hl-line-mode
        (hl-line-mode +1))))

  )

(use-package! hi-lock
  :defer t
  :init
  (global-hi-lock-mode)
  :config
  (setq hi-lock-auto-select-face t)
  )

(use-package! hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  ;; Use a more primitive todo-keyword detection method in major modes that
  ;; don't use/have a valid syntax table entry for comments.
  (add-hook! '(pug-mode-hook haml-mode-hook) #'+hl-todo--use-face-detection-h)
)

(use-package! highlight-indent-guides
  :defer t
  :init
  (defvar highlight-indent-guides-mode nil)
  ;; :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :config
  ;; (when (doom-context-p 'init)
  ;; call hligast late enough to be useful
  ;; (add-hook 'doom-first-buffer-hook #'highlight-indent-guides-auto-set-faces)
  ;; )

  ;; errors when first file is org: (add-hook! 'org-mode-local-vars-hook #'+indent-guides-disable-maybe-h)
  )

(use-package! highlight-parentheses
  :defer t
  :init
  (add-hook! doom-first-buffer
             #'global-highlight-parentheses-mode
             )
  )

(use-package! auto-highlight-symbol
  :commands auto-highlight-symbol-mode
  :init
  (defvar auto-highlight-symbol-mode nil)
  )

(use-package! hilit-chg
  ;; :hook (doom-first-buffer . global-highlight-changes-mode)
  )

(use-package! paren
  ;; highlight matching delimiters
  :hook (doom-first-buffer . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)

  )

(use-package! highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>")
  )
