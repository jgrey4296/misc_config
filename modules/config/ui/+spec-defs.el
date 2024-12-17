;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! modeline
                    "Add values to `global-mode-string` for modeline using mode-line-format"
                    :target global-mode-string
                    :loop 'collect
                    :struct '(TODO)
                    val
                    )

(spec-handling-new! headerline
                    "Add Values to header-line-format"
                    :target header-line-format
                    :loop 'collect
                    val
                    )
