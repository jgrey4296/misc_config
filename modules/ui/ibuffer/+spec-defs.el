;;; +spec-defs.el -*- lexical-binding: t; -*-

(speckler-new! ibuffer-filters
                    "Register ibuffer filters"
                    :target ibuffer-saved-filters
                    :loop 'collect
                    (cons (symbol-name key) val)
                    )

(speckler-new! ibuffer-groups
                    "Register ibuffer groups"
                    :target ibuffer-saved-filter-groups
                    :loop 'collect
                    (cons (symbol-name key) val)
                    )

(speckler-new! ibuffer-formats
                    "Register ibuffer formats"
                    :target ibuffer-formats
                    :loop 'collect
                    val
                    )
