;;; treesit.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defface pasp-integrity-constraint-face
  '((t . (:background "mediumslateblue" )))
  "Integrity constraint for clingo"
  :group 'pasp
  )
(defface pasp-args-face
  '((t . (:background "green")))
  "Arguments of functions"
  :group 'pasp
  )
(defface pasp-statement-face
  '((t . (:background "red")))
  "For statements like #show"
  :group 'pasp
  )

(defvar pasp-ts--font-lock-feature-list
  '(
    ( comment )
    ( basic commands )
    ( rule operators )
    ( overrides )
    )
  )
(defvar pasp-ts--ting-settings '())
(defvar pasp-ts--font-lock-settings '())

;; Comments
(setq pasp-ts--font-lock-settings
      (treesit-font-lock-rules
       :feature 'comment
       :default-language 'clingo
       '((line_comment) @font-lock-comment-face
         (block_comment) @font-lock-comment-face)
       ;; Basic Syntax
       :feature 'basic
       '((number) @font-lock-number-face
         (string) @font-lock-string-face
         (identifier) @font-lock-keyword-face
         ;; todo: fstring
         (function name: (_) @font-lock-doc-face)
         )
       ;; variables
       :feature 'basic
       '((variable) @font-lock-variable-name-face
         (anonymous) @font-lock-comment-face
         )
       ;; operators
       :feature 'operators
       '((default_negation ) @error
         (classical_negation) @error
         )

       ;; Commands
       :feature 'commands
       :override t
       '((["#const" "#show" "#heuristic" "#project" "#program"
           "#include" "#external" "#defined"]  @pasp-statement-face)
         )
       ;; rules
       :feature 'rule
       :override 'append
       '(
         (head (literal)  @font-lock-function-name-face)
         (rule ":-" @font-lock-builtin-face)
         ;; ;;; integrity constraint
         (integrity_constraint ":-" @pasp-integrity-constraint-face)
         ;; ;; aggregation
         (aggregate_function) @error
         ;; conditions
         (condition) @error
         )

       ;; theory?
       ;; -- overrides
       :feature 'basic
       :override 'prepend
       '(
         (((identifier) @error) (:match "_.+?" @error))
         (((identifier) @error) (:match ".+?'" @error))
         (unary_operation operator: _ @error)
         (binary_operation operator: _ @error)
         (comparison (relation) @error)
         (ERROR) @jg-error
         )
       )
      )

;;;###autoload
(defun +jg-pasp-start-treesitter ()
  (when (treesit-ready-p 'pasp)
    (setq font-lock-defaults nil)
    (treesit-parser-create 'pasp)
    (setq-local treesit-font-lock-feature-list pasp-ts--font-lock-feature-list
                treesit-font-lock-settings pasp-ts--font-lock-settings
                treesit-defun-type-regexp (rx (or "function" "class") "_definition")
                ;; imenu-create-index-function #'python-imenu-treesit-create-index
                ;; treesit-defun-name-function #'python--treesit-defun-name
                ;; syntax-propertize-function #'python--treesit-syntax-propertize
                )

    (treesit-minor-mode 1)
    (treesit-major-mode-setup)
    )
  )



;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    October 05, 2025
;; Modified:   October 05, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 30.2))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; treesit.el ends here
