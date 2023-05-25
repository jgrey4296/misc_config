;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! ibuffer-filters ibuffer-saved-filters nil collect
                    (cons (symbol-name key) val)
                    )

(spec-handling-new! ibuffer-groups ibuffer-saved-filter-groups nil collect
                    (cons (symbol-name key) val)
                    )

(spec-handling-new! ibuffer-formats ibuffer-formats nil collect
                    val
                    )
