;;; lang/json/config.el -*- lexical-binding: t; -*-

(after! (evil jg-bindings-total json-mode)
  (load! "+bindings")
  )

(use-package! json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :init
  (when (modulep! +lsp)
    (add-hook 'json-mode-local-vars-hook #'lsp! 'append))
  (when (modulep! +tree-sitter)
    (add-hook! '(json-mode-local-vars-hook
                 jsonc-mode-local-vars-hook)
               :append #'tree-sitter!))
  :config
  (set-electric! 'json-mode :chars '(?\n ?: ?{ ?}))
  )

(use-package! counsel-jq
  :when (modulep! :completion ivy)
  :defer t
)
