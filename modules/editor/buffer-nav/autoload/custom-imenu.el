;;; custom-imenu.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defun jg-custom-imenu-prev-index ()
  " Finds the next definition to put in the buffer index, scanning backward in the buffer from point.
    Should return ‘nil’ if it doesn’t find another definition before point.
    Otherwise it should leave point at the place it finds a definition and return any non-‘nil’ value.
    "

  nil
  )

(defun jg-custom-imenu-name ()
  " return the name for a definition, assuming point is in that
     definition as the ‘imenu-prev-index-position-function’ function
     would leave it."

  nil
  )

(defun custom-imenu-setup-h ()
  "Adapted from dired-imenu"
  (setq-local imenu-prev-index-position-function nil
              imenu-extract-index-name-function nil)
  )



;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    November 27, 2024
;; Modified:   November 27, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; custom-imenu.el ends here
