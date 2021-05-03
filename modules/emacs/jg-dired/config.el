;;; tools/dired/config.el -*- lexical-binding: t; -*-

(after! evil
  (load! "+bindings")
  )
(load! "+vars")
(load! "+funcs")

(use-package! dired-quick-sort
  :commands hydra-dired-quick-sort/body
  )

(use-package! diredfl
  :config
  (set-face-attribute 'diredfl-flag-mark-line nil :background "blueviolet")
)
