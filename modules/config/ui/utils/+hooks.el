;; -*- lexical-binding: t; -*-

(defun +jg-ui-load-advice ()
  (load! "+advice")
  )

(add-hook! 'doom-init-ui-hook    #'rainbow-delimiters-mode)
(add-hook! 'doom-init-ui-hook    #'+jg-ui-load-advice)
(after! ibuffer
  (add-hook! 'ibuffer-mode-hook    #'+jg-ui-ibuffer-update-hook)
  )

(after! helpful
  (add-hook 'helpful-mode-hook
            (defun jg-unset-helpful-dedicated()
              (set-window-dedicated-p (selected-window) nil)))
  )
