;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! ibuffer-filters ibuffer-saved-filters :loop 'collect
                    (cons (symbol-name key) val)
                    )

(spec-handling-new! ibuffer-groups ibuffer-saved-filter-groups :loop 'collect
                    (cons (symbol-name key) val)
                    )

(spec-handling-new! ibuffer-formats ibuffer-formats :loop 'collect
                    val
                    )
