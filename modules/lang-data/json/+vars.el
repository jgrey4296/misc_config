;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! tree-sit-lang
                    '(json-mode       . json)
                    '(jsonc-mode      . json)
                    )

(spec-handling-add! lookup-regular nil
                    '(dired-mode
                     ("Jq Manual" . "https://stedolan.github.io/jq/manual/")
                     )
                    )
