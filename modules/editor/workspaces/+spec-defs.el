;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! projects projectile-project-types nil collect
                    `(,key . ,(apply #'projectile--build-project-plist val))
                    )

(spec-handling-new! project-ignored projectile-globally-ignored-directories nil append
                    val
                    )
