;;; lang/emacs-lisp/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(after! jg-bindings-total
  (message "Setting up lisp bindings")
  (load! "+bindings")
  (load! "+advice")
  )

(use-package! ffap)
(use-package! find-func)

(use-package-hook! emacs-list-mode :post-config
  (defun +jg-lisp-config-hook ()
    (add-hook 'jg-text-whitespace-clean-hook 'delete-trailing-whitespace 10 t)
    (add-hook 'jg-text-whitespace-clean-hook '+jg-lisp-cleanup-ensure-newline 10 t)
    (add-hook 'jg-text-whitespace-clean-hook '+jg-text-cleanup-whitespace 20 t)
    )

  (add-hook! 'emacs-lisp-mode-hook :depth 100
             #'+jg-lisp-config-hook
             )
  )
