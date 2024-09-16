;;; lang/rst/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! sphinx-mode
  :commands sphinx-mode
  :hook (rst-mode . sphinx-mode)
  )

(use-package! rst
  :commands rst-mode
  :config
  (add-hook! 'rst-mode-hook
             #'general-insert-minor-mode
             )
  (setq-hook! 'rst-mode-hook
    yas-indent-line nil)
  )

(use-package! sphinx-doc
  :defer t
  )
