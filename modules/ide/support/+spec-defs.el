;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! flycheck nil :loop 'hook
                    :struct '(:head checker :rest rest)
                    :doc ""
                    (flycheck-mode)
                    (let ((head (plist-get val :head)))
                      (flycheck-select-checker head)
                      (dolist (next (plist-get val :rest))
                        (flycheck-add-next-checker head next 'append)
                        )
                      )
                    )

(spec-handling-new! eglot eglot-server-programs :loop 'collect
                    (cons key val)
                    )

(spec-handling-new! tree-sit-lang
                    ;; for tree-sitter (melpa):
                    tree-sitter-major-mode-language-alist
                    :doc "Match modes to grammars in `tree-sitter-langs-grammar-dir`"
                    :struct '(key-mode . grammar)
                    :loop 'collect
                    `(,key . ,val)
                    )

(spec-handling-new! treesit-lang treesit-load-name-override-list
                    :doc "for treesit (builtin)"
                    :struct '(key-mode :lib-base :entry-func)
                    :loop 'collect
                    `(,key ,(plist-get val :lib-base) ,(plist-get val :entry-func))
                    )

(spec-handling-new! env-handling env-handling-registered
                    :loop 'append
                    val
                    ;; todo (apply #'make-env-handler val)
                    )
