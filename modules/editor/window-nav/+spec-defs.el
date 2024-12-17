;;; +spec-defs.el -*- lexical-binding: t; -*-

(speckler-new-hook! imenu
                    "Register imenu generic expressions"
                    :struct '(strName regexp)
                    (setq-local imenu-generic-expression (append val imenu-generic-expression))
                    )
