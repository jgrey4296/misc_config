;;; config.el -*- lexical-binding: t; -*-

(local-load! "+vars")

(defer-load! jg-bindings-total "+bindings")

(advice-add 'find-function-search-for-symbol :around #'doom--find-function-search-for-symbol-save-excursion-a)
(advice-add 'doom--help-package-configs      :before-until #'+jg-help-package-config-advice)
(advice-add 'doom--help-insert-button        :before-while #'+jg-help-protect-insert-button)

(use-package! free-keys
  :commands (free-keys free-keys-set-prefix)
  :config
  (evil-make-intercept-map free-keys-mode-map)
  )

(use-package! eldoc
  :defer t
  )

(use-package! helpful
  ;; a better *help* buffer
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :init
  ;; Make `apropos' et co search more extensively. They're more useful this way.
  (setq apropos-do-all t)

  (defun jg-unset-helpful-dedicated ()
    (set-window-dedicated-p (selected-window) nil)
    )

  (add-hook 'helpful-mode-hook #'jg-unset-helpful-dedicated)
  (add-hook 'helpful-mode-hook #'outline-minor-mode)

  )

(use-package! info
  :config
  (push (expand-file-name "~/.config/info") Info-directory-list)
  )

(use-package! straight)

;;; config.el ends here
