;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! projects
                    "applys a list of descriptions to `projectile--build-project-plist`"
                    :target projectile-project-types
                    :loop 'collect
                    `(,key . ,(apply #'projectile--build-project-plist val))
                    )

(spec-handling-new! project-ignored
                    "register projects to ignore"
                    :target projectile-globally-ignored-directories
                    :loop 'append
                    val
                    )
