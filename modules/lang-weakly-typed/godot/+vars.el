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
(speckler-add! doc-lookup ()
  `(gdscript-mode
    :documentation #'gdscript-docs-browse-symbol-at-point
    )
  )
(speckler-add! treesit-source ()
  '(gdscript      "git@github.com:PrestonKnopp/tree-sitter-gdscript.git")
  '(glsl          "git@github.com:tree-sitter-grammars/tree-sitter-glsl.git")
  )
(speckler-add! tree-sitter-lang ()
  '(gdscript-mode . gdscript)
  '(gdscript-ts-mode . gdscript)
  '(glsl-mode . glsl)
  )
(speckler-add! fold ()
  :override t
  `(gdscript
    :modes gdscript-mode
    :priority 25
    :triggers (:close     #'+jg-python-close-methods
               :close-all #'outline-hide-sublevels
               :open      #'outline-toggle-children
               :open-all  #'outline-show-all
               :open-rec  #'outline-show-subtree
               :toggle    #'outline-toggle-children
               )
    )
  `(gdscript-ts
    :modes gdscript-ts-mode
    :priority 25
    :triggers (:close     #'+jg-python-close-methods
               :close-all #'outline-hide-sublevels
               :open      #'outline-toggle-children
               :open-all  #'outline-show-all
               :open-rec  #'outline-show-subtree
               :toggle    #'outline-toggle-children
               )
    )
  )
