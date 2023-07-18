;;; +vars.el -*- lexical-binding: t; -*-


(spec-handling-add! file-templates
                    '(gdscript-mode
                      ("\\.gd$"           :trigger "__" :mode gdscript-mode)
                      )
                    )

(spec-handling-add! auto-modes
                    '(godot
                      ("\\.gd\\'" . gdscript-mode)
                      ("\\.tscn\\'" . conf-toml-mode)
                      ("\\.tres\\'" . conf-toml-mode)
                      )
                    )

(spec-handling-add! lookup-handler
                    `(gdscript-mode
                      :documentation ,#'gdscript-docs-browse-symbol-at-point
                      )
                    )
