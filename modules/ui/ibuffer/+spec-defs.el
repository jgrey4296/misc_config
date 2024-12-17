;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! ibuffer-filters
                    "Register ibuffer filters"
                    :target ibuffer-saved-filters
                    :loop 'collect
                    (cons (symbol-name key) val)
                    )

(spec-handling-new! ibuffer-groups
                    "Register ibuffer groups"
                    :target ibuffer-saved-filter-groups
                    :loop 'collect
                    (cons (symbol-name key) val)
                    )

(spec-handling-new! ibuffer-formats
                    "Register ibuffer formats"
                    :target ibuffer-formats
                    :loop 'collect
                    val
                    )
