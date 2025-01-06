;;; +vars.el -*- lexical-binding: t; -*-


(speckler-add! file-templates ()
  '(gdscript-mode
    ("\\.gd$"           :trigger "__" :mode gdscript-mode)
    )
  )

(speckler-add! auto-modes ()
  '(godot
    ("\\.gd\\'" . gdscript-mode)
    ("\\.tscn\\'" . conf-toml-mode)
    ("\\.tres\\'" . conf-toml-mode)
    )
  )

(speckler-add! lookup-handler ()
  `(gdscript-mode
    :documentation ,#'gdscript-docs-browse-symbol-at-point
    )
  )

(speckler-add! treesit-source ()
  '(gdscript      "git@github.com:PrestonKnopp/tree-sitter-gdscript.git")
  '(glsl          "git@github.com:tree-sitter-grammars/tree-sitter-glsl.git")
  )

(speckler-add! treesit-lang ()
  '(gdscript-mode . gdscript)
  '(gdscript-ts-mode . gdscript)
  '(glsl-mode . glsl)
  )
