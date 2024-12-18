;;; +spec-defs.el -*- lexical-binding: t; -*-

(speckler-new! projects (key val)
  "applys a list of descriptions to `projectile--build-project-plist`"
  :target projectile-project-types
  :loop 'collect
  `(,key . ,(apply #'projectile--build-project-plist val))
  )

(speckler-new! project-ignored (key val)
  "register projects to ignore"
  :target projectile-globally-ignored-directories
  :loop 'append
  val
  )
