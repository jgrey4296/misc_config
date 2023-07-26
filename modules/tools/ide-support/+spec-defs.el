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
