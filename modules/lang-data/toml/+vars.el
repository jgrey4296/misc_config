;;; +vars.el -*- lexical-binding: t; -*-

(after! jg-ui-reapply-hook-ready
  (pushnew! projectile-project-root-files "dooter.py" "doot.toml")
  (+jg-projects-add-spec 'jg-toml-project '(("dooter.py") :project-file "doot.toml" :related-files-fn +jg-toml-related-files-fn))
  )
