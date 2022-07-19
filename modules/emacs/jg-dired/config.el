;;; tools/dired/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")

(use-package-hook! dired :post-config
  (load! "+bindings")
)

(use-package! dired-quick-sort
  :commands hydra-dired-quick-sort/body
  )

(use-package! diredfl
  :config
  (set-face-attribute 'diredfl-flag-mark-line nil :background "blueviolet")
)
