;; +colours.el -*- mode: eLisp; lexical-binding: t; -*-

(use-package! palette-mode
  :commands palette-mode
  )

(use-package! rainbow-mode
  :defer t
)

(use-package! rainbow-delimiters
  :config
  (add-hook! 'prog-mode-hook  #'rainbow-delimiters-mode)
  (setq rainbow-delimiters-max-face-count 4)
  )
