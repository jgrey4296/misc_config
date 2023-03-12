;;; +vars.el -*- lexical-binding: t; -*-

(setq-default jg-godot-doc-url "https://docs.godotengine.org/en/stable/"

              )

;;-- file templates
(after! jg-completion-templates
  ;; gdscript-mode
  (+jg-completion-add-file-templates
   'gdscript-mode
   '(
     ("\\.gd$"           :trigger "__" :mode gdscript-mode)
     )
   )
  )
;;-- end file templates
