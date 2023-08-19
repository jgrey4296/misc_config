;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! modeline global-mode-string
                    :loop 'collect
                    :doc "Add values to `global-mode-string` for modeline using mode-line-format"
                    :struct '(TODO)
                    val
                    )
