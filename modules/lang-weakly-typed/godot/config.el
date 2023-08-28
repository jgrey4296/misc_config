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

(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(after! projectile
  (add-to-list 'projectile-project-root-files "project.godot"))

(use-package! gdscript-mode
  :commands gdscript-mode
  )
;;; config.el ends here
