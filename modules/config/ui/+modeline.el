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
  (setq doom-modeline-enable-word-count t
        doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode text-mode)
        doom-modeline-project-name t
        doom-modeline-repl t
        doom-modeline-lsp t
        doom-modeline-minor-modes t
        doom-modeline-indent-info t
        doom-modeline-buffer-encoding t
        doom-modeline-env-enable-python nil
        doom-modeline-mode-alist'(
                                  (message-mode         . message)
                                  (git-commit-mode      . message)
                                  (magit-mode           . vcs)
                                  (dashboard-mode       . dashboard)
                                  (Info-mode            . info)
                                  (image-mode           . media)
                                  (pdf-view-mode        . pdf)
                                  (org-src-mode         . org-src)
                                  (paradox-menu-mode    . package)
                                  (xwidget-webkit-mode  . minimal)
                                  (git-timemachine-mode . timemachine)
                                  (calc-mode            . calculator)
                                  (calc-trail-mode      . calculator)
                                  (circe-mode           . special)
                                  (erc-mode             . special)
                                  (dired-mode           . dired)
                                  )
        )

  (add-to-list 'doom-modeline-mode-alist          '(+doom-dashboard-mode . dashboard))
  (add-hook    'after-setting-font-hook          #'+modeline-resize-for-font-h)
  (add-hook    'doom-load-theme-hook             #'doom-modeline-refresh-bars)
  (add-hook    'magit-mode-hook                  #'+modeline-hide-in-non-status-buffer-h)
  (add-hook    'pre-redisplay-functions          #'jg-ui-modeline-update-marked-count-h)

  (advice-add 'ws-butler-after-save          :around #'+modeline--inhibit-modification-hooks-a)
  (advice-add 'doom-modeline-propertize-icon :around #'+modeline-disable-icon-in-daemon-a)
  (advice-add 'doom-modeline-set-modeline    :after #'+jg-ui-modeline-record-ad)

  (local-load! "utils/+modeline")

  (add-hook 'speckler-hook #'+jg-ui-reset-modeline-default)
  )

(use-package! hide-mode-line
  :config
  (add-hook 'completion-list-mode-hook #'hide-mode-line-mode)
  ;; (add-hook 'Man-mode-hook             #'hide-mode-line-mode)
  )
