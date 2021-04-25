;;; tools/dired/config.el -*- lexical-binding: t; -*-

(load! "+bindings")
(load! "+vars")
(load! "+funcs")

(use-package! dired-quick-sort
  :commands hydra-dired-quick-sort/body
  )

(add-hook! dired-load
           #'+jg-dired-binding-hook
           #'+jg-dired-var-hook
           )
