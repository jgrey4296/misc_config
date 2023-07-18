;;; lang/graphql/config.el -*- lexical-binding: t; -*-

(load! "+vars")

(use-package! graphql-mode
  :commands graphql-mode
  :init
  (add-hook 'graphql-mode-hook #'rainbow-delimiters-mode)
  )

(use-package! graphql-doc
  :after graphql-mode)
