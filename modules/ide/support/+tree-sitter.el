;;; +treesitter.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(use-package! tree-sitter ;; Melpas
  :disabled t
  :config
  (require 'tree-sitter-langs)
  )

(use-package! tree-sitter-langs
  :defer t
  :config
  ;; possibly: (cl-pushnew (tree-sitter-langs--bin-dir) tree-sitter-load-path :test #'string-equal)
  )

(speckler-setq! tree-sitter ()
  tree-sitter-load-path (list
                         (expand-file-name (format "straight/%s/tree-sitter-langs/bin/" straight-build-dir) doom-local-dir)
                         (expand-file-name "~/.local/tree-sitter/")
                         )
)

(use-package! ts-fold
  :after tree-sitter
  )

(speckler-new! tree-sitter-lang (key val)
  "Match modes to grammars in `tree-sitter-langs-grammar-dir`"
  :target tree-sitter-major-mode-language-alist
  :struct '(key-mode . grammar)
  :loop 'collect
  `(,key . ,val)
  )

(speckler-add! tree-sitter-lang ()
  '(agda-mode       . agda)
  '(c-mode          . c)
  '(c++-mode        . cpp)
  '(elm-mode        . elm)
  '(julia-mode      . julia)
  '(ruby-mode       . ruby)
  '(tuareg-mode     . ocaml)
  )

;;todo: use treesit-font-lock-rules
;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 10, 2024
;; Modified:   September 10, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +treesitter.el ends here
