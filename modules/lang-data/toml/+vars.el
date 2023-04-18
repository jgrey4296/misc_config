;;; +vars.el -*- lexical-binding: t; -*-

(pushnew! projectile-project-root-files "dooter.py" "doot.toml")
(spec-handling-add! projects nil
                    ('jg-toml-project ("dooter.py") :project-file "doot.toml" :related-files-fn +jg-toml-related-files-fn)
                    )
