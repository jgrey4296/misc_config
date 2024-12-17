;;; +spec-defs.el -*- lexical-binding: t; -*-

(speckler-new! evil-initial
                    "Set initial evil states for modes"
                    :loop 'do
                    (evil-set-initial-state key (car (ensure-list val)))
                    )
