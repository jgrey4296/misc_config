;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! projects projectile-project-types :loop 'collect
                    `(,key . ,(apply #'projectile--build-project-plist val))
                    )

(spec-handling-new! project-ignored projectile-globally-ignored-directories :loop 'append
                    val
                    )

(spec-handling-new! compile-commands counsel-compile-local-builds :loop 'append
                    val
                    )
