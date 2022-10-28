
(after! jg-bindings-total
  (load! "+bindings")
  )

(use-package! rst
  :config
  (add-hook 'rst-mode-hook #'(lambda ()
                               (setq-local yas-indent-line nil)
                               ))
  )



;;; config.el ends here
