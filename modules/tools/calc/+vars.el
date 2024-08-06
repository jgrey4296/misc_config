;;; +vars.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;-- keymaps
(defvar jg-calc-mode-map (make-sparse-keymap))

(defvar jg-calc-trail-mode-map (make-sparse-keymap))

(defvar jg-calc-dispatch-map (make-sparse-keymap))

;;-- end keymaps


(spec-handling-add! popup
                    '(calc
                      ("^\\*Calc"                                              :vslot -7 :side bottom :size 0.4 :select t :quit nil :ttl 0 :priority -100)
                      )
                    )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    February 02, 2024
;; Modified:   February 02, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +vars.el ends here
