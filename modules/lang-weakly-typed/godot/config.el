;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: November 14, 2022
;; Modified: November 14, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(load! "+vars")
(after! jg-bindings-total
  (load! "+bindings")
  )

(after! projectile
  (add-to-list 'projectile-project-root-files "project.godot"))

;;
;;; Packages

(use-package! gdscript-mode
  :defer t
  :config
  (set-lookup-handlers! 'gdscript-mode
    :documentation #'gdscript-docs-browse-symbol-at-point)

  (when (modulep! +lsp)
    (add-hook 'gdscript-mode-local-vars-hook #'lsp! 'append))

  (map! :localleader
        :map gdscript-mode-map

        (:prefix ("r" . "run")
         :desc "Open project in Godot" "e" #'gdscript-godot-open-project-in-editor
         :desc "Run project" "p" #'gdscript-godot-run-project
         :desc "Run debug" "d" #'gdscript-godot-run-project-debug
         :desc "Run current scene" "s" #'gdscript-godot-run-current-scene)

        (:prefix ("d" . "debug")
         :desc "Add breakpoint" "a"  #'gdscript-debug-add-breakpoint
         :desc "Display breakpoint buffer" "b" #'gdscript-debug-display-breakpoint-buffer
         :desc "Remove breakpoint" "d" #'gdscript-debug-remove-breakpoint
         :desc "Continue execution" "c" #'gdscript-debug-continue
         :desc "Next" "n" #'gdscript-debug-next
         :desc "Step" "s" #'gdscript-debug-step)

        (:prefix ("h" . "help")
         :desc "Browse online API" "b" #'gdscript-docs-browse-api
         :desc "Browse API at point" "f" #'gdscript-docs-browse-symbol-at-point)

        (:prefix ("f" . "format")
         :desc "Format buffer" "b" #'gdscript-format-buffer
         :desc "Format region" "r" #'gdscript-format-region)))
;;; config.el ends here
