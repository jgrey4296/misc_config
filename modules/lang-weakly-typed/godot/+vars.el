;;; +vars.el -*- lexical-binding: t; -*-


(speckler-add! file-templates
                    '(gdscript-mode
                      ("\\.gd$"           :trigger "__" :mode gdscript-mode)
                      )
                    )

(speckler-add! auto-modes
                    '(godot
                      ("\\.gd\\'" . gdscript-mode)
                      ("\\.tscn\\'" . conf-toml-mode)
                      ("\\.tres\\'" . conf-toml-mode)
                      )
                    )

(speckler-add! lookup-handler
                    `(gdscript-mode
                      :documentation ,#'gdscript-docs-browse-symbol-at-point
                      )
                    )
