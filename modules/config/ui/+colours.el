;; +colours.el -*- mode: eLisp; lexical-binding: t; -*-

(use-package! palette-mode
  :commands palette-mode
  )

(use-package! rainbow-mode
  :defer t
  :init
  (add-hook! 'prog-mode-hook 'rainbow-mode)
)

(use-package! rainbow-delimiters
  :config
  (add-hook! 'doom-init-ui-hook  #'rainbow-delimiters-mode)
  (setq rainbow-delimiters-max-face-count 4)
  )
