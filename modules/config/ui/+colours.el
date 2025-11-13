;; +colours.el -*- mode: eLisp; lexical-binding: t; -*-

(use-package! rainbow-mode
  :defer t
  :init
  (add-hook! 'prog-mode-hook  #'rainbow-mode)
)

(use-package! rainbow-delimiters
  :defer t
  :init
  (add-hook! 'highlight-parentheses-mode-hook  #'rainbow-delimiters-mode)
  (setq rainbow-delimiters-max-face-count 4)
  )
