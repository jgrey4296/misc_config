;;; lang/json/config.el -*- lexical-binding: t; -*-

(after! (jg-bindings-total json-mode)
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
  (add-hook! 'json-mode-hook 'hs-minor-mode)
  )

(use-package! counsel-jq
  :when (modulep! :completion ivy)
  :defer t
)

(spec-handling-add! hideshow t
                    `(json
                      (json-mode ,(rx (| "[" "{") line-end) ,(rx (| "]" "}") (opt ",") line-end))
                     )
                    )
(spec-handling-add! fold t
                    '(json
                     :modes (json-mode)
                     :priority 25
                     :triggers (:open-all   hs-show-all
                                :close-all  hs-hide-all
                                :toggle     hs-toggle-hiding
                                :open       hs-show-block
                                :open-rec   nil
                                :close      hs-hide-block
                                )
                     )
                    )
