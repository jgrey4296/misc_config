;;; +vars.el -*- lexical-binding: t; -*-

;;-- projectile
(after! projectile
  (pushnew! projectile-project-root-files "dooter.py" "doot.toml")
  (projectile-register-project-type 'jg-toml-project '("dooter.py")
                                    :project-file "doot.toml"
                                    :related-files-fn #'+jg-toml-related-files-fn
                                    )
  )
;;-- end projectile
