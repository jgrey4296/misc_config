;;; +spec-defs.el -*- lexical-binding: t; -*-


(spec-handling-new! flycheck nil :loop 'hook
                    :struct '(:head checker :rest rest)
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
                    tree-sitter-major-mode-language-alist
                    :doc "Match modes to grammars in `tree-sitter-langs-grammar-dir`"
                    :struct '(key-mode . grammar)
                    :loop 'collect
                    `(,key . ,val)
                    )
