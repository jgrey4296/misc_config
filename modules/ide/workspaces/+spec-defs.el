;;; +spec-defs.el -*- lexical-binding: t; -*-

(speckler-new! projects (key val)
  "applys a list of descriptions to `projectile--build-project-plist`"
  :target projectile-project-types
  :loop 'collect
  (cons key (apply #'projectile--build-project-plist (mapcar #'upfun! val)))
  )

(speckler-new! project-ignored (key val)
  "register projects to ignore"
  :target projectile-globally-ignored-directories
  :loop 'append
  val
  )
