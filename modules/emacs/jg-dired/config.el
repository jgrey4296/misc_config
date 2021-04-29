;;; tools/dired/config.el -*- lexical-binding: t; -*-

(after! evil
  (load! "+bindings")
  )
(load! "+vars")
(load! "+funcs")

(use-package! dired-quick-sort
  :commands hydra-dired-quick-sort/body
  )


(add-hook 'dired-load-hook #'+jg-dired-binding-hook)
(add-hook 'dired-load-hook #'+jg-dired-var-hook)
