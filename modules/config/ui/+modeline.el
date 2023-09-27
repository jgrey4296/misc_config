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
  (add-to-list 'doom-modeline-mode-alist '(+doom-dashboard-mode . dashboard))
  (add-hook    'after-setting-font-hook #'+modeline-resize-for-font-h)
  (add-hook    'doom-load-theme-hook    #'doom-modeline-refresh-bars)
  (add-hook    'magit-mode-hook         #'+modeline-hide-in-non-status-buffer-h)
  )

(use-package! hide-mode-line
  :config
  (add-hook 'completion-list-mode-hook #'hide-mode-line-mode)
  (add-hook 'Man-mode-hook             #'hide-mode-line-mode)
  )
