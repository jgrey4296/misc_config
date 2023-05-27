;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! modeline global-mode-string :loop 'collect
                    ;; formatted as mode-line-format specifies
                    val
                    )
