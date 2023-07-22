;;; +vars.el -*- lexical-binding: t; -*-

(setq jg-popup-ivy-predicate-patterns (rx (or "*helpful" "*helm-" "doom" "*dired-log" "magit" "*Free Keys")))

(spec-handling-add! popup
                    '(defaults
                       ("*jg-customised*" :priority -200)
                       )
                    )
