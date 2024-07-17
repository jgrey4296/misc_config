;;; treesit.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;; :feature maps to treesit-font-lock-feature-list
;; :override allows refontifying
;; capture names in query map to face names
;; capture names can be function names for fn(node override start end rest)

(defvar python--treesit-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'python
   '((comment) @font-lock-comment-face)

   :feature 'string
   :language 'python
   '((string) @python--treesit-fontify-string)

   ;; HACK: This feature must come after the string feature and before
   ;; other features.  Maybe we should make string-interpolation an
   ;; option rather than a feature.
   :feature 'string-interpolation
   :language 'python
   '((interpolation) @python--treesit-fontify-string-interpolation)

   :feature 'keyword
   :language 'python
   `([,@python--treesit-keywords] @font-lock-keyword-face
     ((identifier) @font-lock-keyword-face
      (:match "\\`self\\'" @font-lock-keyword-face)))

   :feature 'definition
   :language 'python
   '((function_definition
      name: (identifier) @font-lock-function-name-face)
     (class_definition
      name: (identifier) @font-lock-type-face)
     (parameters (identifier) @font-lock-variable-name-face))

   :feature 'function
   :language 'python
   '((call function: (identifier) @font-lock-function-call-face)
     (call function: (attribute
                      attribute: (identifier) @font-lock-function-call-face)))

   :feature 'builtin
   :language 'python
   `(((identifier) @font-lock-builtin-face
      (:match ,(rx-to-string
                `(seq bol
                  (or ,@python--treesit-builtins
                      ,@python--treesit-special-attributes)
                  eol))
              @font-lock-builtin-face)))

   :feature 'constant
   :language 'python
   '([(true) (false) (none)] @font-lock-constant-face)

   :feature 'assignment
   :language 'python
   `(;; Variable names and LHS.
     (assignment left: (identifier)
                 @font-lock-variable-name-face)
     (assignment left: (attribute
                        attribute: (identifier)
                        @font-lock-property-use-face))
     (pattern_list (identifier)
                   @font-lock-variable-name-face)
     (tuple_pattern (identifier)
                    @font-lock-variable-name-face)
     (list_pattern (identifier)
                   @font-lock-variable-name-face)
     (list_splat_pattern (identifier)
                         @font-lock-variable-name-face))

   :feature 'decorator
   :language 'python
   '((decorator "@" @font-lock-type-face)
     (decorator (call function: (identifier) @font-lock-type-face))
     (decorator (identifier) @font-lock-type-face))

   :feature 'type
   :language 'python
   `(((identifier) @font-lock-type-face
      (:match ,(rx-to-string
                `(seq bol (or ,@python--treesit-exceptions)
                  eol))
              @font-lock-type-face))
     (type (identifier) @font-lock-type-face))

   :feature 'escape-sequence
   :language 'python
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :feature 'number
   :language 'python
   '([(integer) (float)] @font-lock-number-face)

   :feature 'property
   :language 'python
   '((attribute
      attribute: (identifier) @font-lock-property-use-face)
     (class_definition
      body: (block
             (expression_statement
              (assignment left:
                          (identifier) @font-lock-property-use-face)))))

   :feature 'operator
   :language 'python
   `([,@python--treesit-operators] @font-lock-operator-face)

   :feature 'bracket
   :language 'python
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :feature 'delimiter
   :language 'python
   '(["," "." ":" ";" (ellipsis)] @font-lock-delimiter-face)

   :feature 'variable
   :language 'python
   '((identifier) @python--treesit-fontify-variable)
   )
)


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 05, 2024
;; Modified:   July 05, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; treesit.el ends here
