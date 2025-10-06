;;; +vars.el -*- lexical-binding: t; -*-

(speckler-add! tree-sitter-lang ()
  '(json-mode       . json)
  '(jsonc-mode      . json)
  '(json-ts-mode    . json)
  )
(speckler-add! treesit-source ()
  '(json          "git@github.com:tree-sitter/tree-sitter-json.git")
  )
(speckler-add! auto-modes ()
  '(json
    ( "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'" . json-mode)
    )
  )
(speckler-add! hideshow ()
  `(json
    (json-mode ,(rx (| "[" "{") line-end) ,(rx (| "]" "}") (opt ",") line-end))
    )
  )
(speckler-add! fold ()
  `(json
    :modes (json-mode)
    :priority 25
    :triggers (:open-all   hs-show-all
               :close-all  #'(lambda () (hs-hide-level 0))
               :toggle     hs-toggle-hiding
               :open       hs-show-block
               :open-rec   nil
               :close      hs-hide-block
               )
    )
  )

(speckler-add! electric ()
  '(json-mode :chars (?\n ?: ?{ ?}))
  )
