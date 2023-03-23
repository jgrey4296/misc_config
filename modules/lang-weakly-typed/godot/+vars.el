;;; +vars.el -*- lexical-binding: t; -*-

(setq-default jg-godot-doc-url "https://docs.godotengine.org/en/stable/"

              )

;;-- file templates
(after! jg-ui-reapply-hook-ready
  (+jg-snippets-add-file-spec
   'gdscript-mode
   '(
     ("\\.gd$"           :trigger "__" :mode gdscript-mode)
     )
   )
  )
;;-- end file templates
