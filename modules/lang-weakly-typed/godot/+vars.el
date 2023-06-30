;;; +vars.el -*- lexical-binding: t; -*-


(spec-handling-add! file-templates
                    '(gdscript-mode
                     ("\\.gd$"           :trigger "__" :mode gdscript-mode)
                     )
                    )
(spec-handling-add! lookup-regular
                    '(gdscript-mode
                     ("Godot Docs" . "https://docs.godotengine.org/en/stable/")
                     ("GdScript Reference" . "https://docs.godotengine.org/en/stable/tutorials/scripting/gdscript/gdscript_basics.html" )
                     ("GDScript" . "https://gdscript.com/solutions/")
                     ("GuT" . "https://bitwes.github.io/GutWiki/Godot4/Home.html")
                     ("GdUnit" . "https://mikeschulze.github.io/gdUnit4/")
                     ("Shaders" . "https://www.ronja-tutorials.com/")
                     ("Godot Shaders" . "https://godotshaders.com/")
                     )
                    )

(spec-handling-add! auto-modes
                    '(godot
                      ("\\.gd\\'" . gdscript-mode)
                      ("\\.tscn\\'" . conf-toml-mode)
                      ("\\.tres\\'" . conf-toml-mode)
                      )
                    )
