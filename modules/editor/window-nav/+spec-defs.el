;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! imenu nil
                    :struct '(strName regexp)
                    :loop 'hook
                    (setq-local imenu-generic-expression (append val imenu-generic-expression))
                    )
