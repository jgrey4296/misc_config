;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! evil-initial nil :loop 'do
                    (evil-initial-state key (car (ensure-list val)))
                    )
