;;; +vars.el -*- lexical-binding: t; -*-


;;-- file templates
(spec-handling-add! file-templates nil
                    ('gdscript-mode
                     ("\\.gd$"           :trigger "__" :mode gdscript-mode)
                     )
                    )
(spec-handling-add! lookup-regular nil
                    (gdscript-mode
                     ("Godot Docs" . "https://docs.godotengine.org/en/stable/")
                     ("GdScript Reference" . "https://docs.godotengine.org/en/stable/tutorials/scripting/gdscript/gdscript_basics.html" )
                     )
                    )
;;-- end file templates
