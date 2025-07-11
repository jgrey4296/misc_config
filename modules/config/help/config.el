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
  :hook (prog-mode . eldoc-mode)
  :config
  (global-eldoc-mode -1)
  (setq eldoc-idle-delay 0.5
        eldoc-echo-area-prefer-doc-buffer nil
        eldoc-display-functions (list #'eldoc-display-in-echo-area)
        eldoc-echo-area-use-multiline-p nil
        )
  )

(use-package! helpful
  ;; a better *help* buffer
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :init
  (defun jg-unset-helpful-dedicated ()
    (set-window-dedicated-p (selected-window) nil)
    )

  (add-hook 'helpful-mode-hook #'jg-unset-helpful-dedicated)
  ;; (add-hook 'helpful-mode-hook #'hs-minor-mode)

  (when (modulep! :ui ivy)
    (setq counsel-describe-function-function #'helpful-callable
          counsel-describe-variable-function #'helpful-variable
          counsel-descbinds-function         #'helpful-callable
          )
    )
  (setq helpful-max-buffers 5)
  )

(use-package! info
  :config
  (push (expand-file-name "~/.config/info") Info-directory-list)
  )

(use-package! straight)

;;; config.el ends here
