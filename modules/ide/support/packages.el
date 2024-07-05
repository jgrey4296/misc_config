;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! eglot)
(package! flycheck-eglot)

(package! lsp-mode)
(package! lsp-ui)
(package! lsp-ivy)

(package! cedet :built-in 'prefer)
(package! semantic)

(package! flycheck)
(package! flycheck-popup-tip)
(package! flycheck-posframe)

(package! treesit :built-in t)
(package! tree-sitter)
(package! tree-sitter-indent)
(package! tree-sitter-langs)

(package! lint-result-mode :recipe (:host github :repo "jgrey4296/misc-modes" :files ("major-modes/lint-result-mode/*.el")))
