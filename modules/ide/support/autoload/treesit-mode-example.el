;;; treesit-mode-example.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

#'treesit-ready-p
#'treesit-parser-create
'treesit-font-lock-settings
'treesit-font-lock-level
'treesit-simple-indent-rules
'treesit-defun-type-regexp
'treesit-defun-name-function
'treesit-simple-imenu-settings
#'treeesit-major-mode-setup


;;;###autoload
(define-derived-mode python-ts-mode python-base-mode "Python"
  "Major mode for editing Python files, using built in treesit library.

\\{python-ts-mode-map}"
  :syntax-table python-mode-syntax-table
  (when (treesit-ready-p 'python)
    (treesit-parser-create 'python)
    (setq-local treesit-font-lock-feature-list
                '(( comment definition)
                  ( keyword string type)
                  ( assignment builtin constant decorator
                    escape-sequence number property string-interpolation )
                  ( bracket delimiter function operator variable)))
    (setq-local treesit-font-lock-settings python--treesit-settings)
    (setq-local imenu-create-index-function
                #'python-imenu-treesit-create-index)
    (setq-local treesit-defun-type-regexp (rx (or "function" "class")
                                              "_definition"))
    (setq-local treesit-defun-name-function
                #'python--treesit-defun-name)
    (treesit-major-mode-setup)

    (python-skeleton-add-menu-items)

    (when python-indent-guess-indent-offset
      (python-indent-guess-indent-offset))

    )
 )



;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 02, 2024
;; Modified:   September 02, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; treesit-mode-example.el ends here
