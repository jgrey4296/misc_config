;;; lang/json/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(defer-load! (jg-bindings-total json-mode) "+bindings")

(use-package! json-mode
  :commands json-mode
  :init
  (add-hook! '(json-mode-local-vars-hook
               jsonc-mode-local-vars-hook)
             :append #'tree-sitter!)

  :config
  (add-hook! 'json-mode-hook 'hs-minor-mode)
  )

(use-package! counsel-jq
  :defer t
)

(use-package! jq-mode
  :after json-mode
  )
