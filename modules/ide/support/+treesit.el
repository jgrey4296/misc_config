;;; +treesitter.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(advice-add 'treesit-major-mode-setup :after #'+jg-support-treesit-update-fontlock-a)

(use-package! treesit ;; builtin
  :defer t
  :init
  (add-hook 'jg-ui-transient-toggles-hook  #'+jg-support-build-treesit-transient 99)
  :config
  (require 'tree-sitter-langs)
  ;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
  )

(use-package! tree-sitter-langs
  :defer t
  :config
  ;; possibly: (cl-pushnew (tree-sitter-langs--bin-dir) tree-sitter-load-path :test #'string-equal)
  )

(speckler-setq! treesit ()
  treesit-extra-load-path (list
                           (expand-file-name (format "straight/%s/tree-sitter-langs/bin/" straight-build-dir) doom-local-dir)
                           (expand-file-name "~/.local/tree-sitter/")
                           )
  treesit-font-lock-level 3

  ;; treesit-simple-indent-rules
  ;; treesit-defun-type-regexp
  ;; treesit-defun-name-function
  ;; treesit-simple-imenu-settings
  ;; treesit-max-buffer-size
  ;;
)

(speckler-new! treesit-source (key val)
  "Set the pairings of language to grammar"
  :target treesit-language-source-alist
  :struct '(lang . (url revision source-dir CC C++))
  :loop 'collect
  (cons key val)
  )

(speckler-new! treesit-bin-override (key val)
  "for treesit (builtin)"
  :target treesit-load-name-override-list
  :struct '(key-mode :lib-base :entry-func)
  :loop 'collect
  `(,key ,(plist-get val :lib-base) ,(plist-get val :entry-func))
  )

(speckler-add! popup ()
  '(treesit
    ("^\\*treesit explorer\\*" :side right :select nil :priority 0)
   )
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
