;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! tree-sit-lang
                    '(json-mode       . json)
                    '(jsonc-mode      . json)
                    )

(spec-handling-add! lookup-regular
                    '(dired-mode
                      ("Jq Manual" . "https://stedolan.github.io/jq/manual/")
                      )
                    )

(spec-handling-add! auto-modes
                    '(json
                      ( "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'" . json-mode)
                      )
                    )

(spec-handling-add! hideshow
                    `(json
                      (json-mode ,(rx (| "[" "{") line-end) ,(rx (| "]" "}") (opt ",") line-end))
                      )
                    )

(spec-handling-add! fold
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

(spec-handling-add! electric
                    '(json-mode :chars (?\n ?: ?{ ?}))
                    )
