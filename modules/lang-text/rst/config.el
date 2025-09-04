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
             #'librarian-insert-minor-mode
             )
  (setq-hook! 'rst-mode-hook
    yas-indent-line nil)
  )

(use-package! sphinx-doc
  :defer t
  )

(use-package! jinja2-mode
  :config

  (add-hook 'jinja2-mode-hook #'abbrev-mode)
  (add-hook 'jinja2-mode-hook #'librarian-insert-minor-mode)
  )
