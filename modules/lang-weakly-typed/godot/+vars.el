;;; +vars.el -*- lexical-binding: t; -*-

(setq-default jg-godot-doc-url "https://docs.godotengine.org/en/stable/"

              )

;;-- file templates
(spec-handling-add! file-templates nil
                    ('gdscript-mode
                     ("\\.gd$"           :trigger "__" :mode gdscript-mode)
                     )
                    )
;;-- end file templates
