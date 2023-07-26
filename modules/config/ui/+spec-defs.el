;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! modeline global-mode-string
                    :loop 'collect
                    :doc "Add values to `global-mode-string` for modeline using mode-line-format"
                    :struct '(TODO)
                    val
                    )

(spec-handling-new! tree-sit-lang
                    tree-sitter-major-mode-language-alist
                    :doc "Match modes to grammars in `tree-sitter-langs-grammar-dir`"
                    :struct '(key-mode . grammar)
                    :loop 'collect
                    `(,key . ,val)
                    )
