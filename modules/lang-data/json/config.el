;;; lang/json/config.el -*- lexical-binding: t; -*-

(defer-load! (jg-bindings-total json-mode) "+bindings")

(use-package! json-mode
  :init
  (when (modulep! +lsp)
    (add-hook 'json-mode-local-vars-hook #'lsp! 'append))
  (when (modulep! +tree-sitter)
    (add-hook! '(json-mode-local-vars-hook
                 jsonc-mode-local-vars-hook)
               :append #'tree-sitter!))
  :config
  (set-electric! 'json-mode :chars '(?\n ?: ?{ ?}))
  (add-hook! 'json-mode-hook 'hs-minor-mode)
  )

(use-package! counsel-jq
  :when (modulep! :completion ivy)
  :defer t
)
