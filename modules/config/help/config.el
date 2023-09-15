;;; config.el -*- lexical-binding: t; -*-

(local-load! "+vars")

(defer-load! jg-bindings-total "+bindings")

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

  (defun doom-use-helpful-a (fn &rest args)
    "Force FN to use helpful instead of the old describe-* commands."
    (letf! ((#'describe-function #'helpful-function)
            (#'describe-variable #'helpful-variable))
      (apply fn args)))

  (defun jg-unset-helpful-dedicated()
    (set-window-dedicated-p (selected-window) nil)
    )


  (add-hook 'helpful-mode-hook #'jg-unset-helpful-dedicated)
  (add-hook 'helpful-mode-hook #'outline-minor-mode)

  )

;;; config.el ends here
