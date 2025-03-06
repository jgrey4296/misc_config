;; +modeline.el -*- mode: eLisp; lexical-binding: t; -*-

(use-package! all-the-icons
  :disabled t)

(use-package! doom-modeline
  :hook (doom-init-ui . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :init
  (defface doom-modeline-buffer-modified
    '((t (:inherit (error bold) :background unspecified)))
    "Face used for the \\='unsaved\\=' symbol in the mode-line."
    :group 'doom-modeline-faces)

  :config
  (local-load! "utils/+modeline")
  (setq doom-modeline-enable-word-count t
        doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode text-mode)
        doom-modeline-project-name t
        doom-modeline--debug-dap t
        doom-modeline-repl t
        doom-modeline-lsp t
        doom-modeline-minor-modes t
        doom-modeline-indent-info t
        doom-modeline-buffer-encoding t
        )

  (add-to-list 'doom-modeline-mode-alist '(+doom-dashboard-mode . dashboard))
  (add-hook    'after-setting-font-hook #'+modeline-resize-for-font-h)
  (add-hook    'doom-load-theme-hook    #'doom-modeline-refresh-bars)
  (add-hook    'magit-mode-hook         #'+modeline-hide-in-non-status-buffer-h)

  (advice-add 'doom-modeline-set-modeline :after #'+jg-ui-modeline-record-ad)
  (doom-modeline-set-modeline 'main-alt)
  )

(use-package! hide-mode-line
  :config
  (add-hook 'completion-list-mode-hook #'hide-mode-line-mode)
  ;; (add-hook 'Man-mode-hook             #'hide-mode-line-mode)
  )
