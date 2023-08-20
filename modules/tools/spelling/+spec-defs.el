;;; +spec-defs.el -*- lexical-binding: t; -*-


(spec-handling-new! flyspell-predicate :loop 'hook
                    (flyspell-mode)
                    (setq-local flyspell-generic-check-word-predicate val)
                    )

(spec-handling-add! flyspell-predicate
                    `(markdown-mode ,#'+markdown-flyspell-word-p)
                    `(gfm-mode ,#'+markdown-flyspell-word-p)
                    )
