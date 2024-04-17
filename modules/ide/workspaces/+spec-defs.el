;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! projects projectile-project-types :loop 'collect
                    :doc "applys a list of descriptions to `projectile--build-project-plist`"
                    `(,key . ,(apply #'projectile--build-project-plist val))
                    )

(spec-handling-new! project-ignored projectile-globally-ignored-directories :loop 'append
                    val
                    )
