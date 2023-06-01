;;; config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(after! jg-bindings-total
  (load! "+bindings")
  )
(after! ivy
  (load! "utils/+ivys")
  )
(after! helpful
  (add-hook! 'helpful-mode-hook
            (defun jg-unset-helpful-dedicated()
              (set-window-dedicated-p (selected-window) nil))
            #'outline-minor-mode
            )
  )
(defer! 120
  (+jg-help-load-package-list)
  )

(use-package! free-keys
  :commands (free-keys free-keys-set-prefix)
  :config
  (evil-make-intercept-map free-keys-mode-map)
  )

;;; config.el ends here
