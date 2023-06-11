;;; config.el -*- lexical-binding: t; -*-

(load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(after! helpful
  (add-hook! 'helpful-mode-hook
            (defun jg-unset-helpful-dedicated()
              (set-window-dedicated-p (selected-window) nil))
            #'outline-minor-mode
            )
  )


(use-package! free-keys
  :commands (free-keys free-keys-set-prefix)
  :config
  (evil-make-intercept-map free-keys-mode-map)
  )

;;; config.el ends here
