;;; +spec-defs.el -*- lexical-binding: t; -*-

(speckler-new! projects
                    "applys a list of descriptions to `projectile--build-project-plist`"
                    :target projectile-project-types
                    :loop 'collect
                    `(,key . ,(apply #'projectile--build-project-plist val))
                    )

(speckler-new! project-ignored
                    "register projects to ignore"
                    :target projectile-globally-ignored-directories
                    :loop 'append
                    val
                    )
