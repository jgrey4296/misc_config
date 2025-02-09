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

(package! treesit :built-in t)
(package! tree-sitter)
(package! tree-sitter-indent)
(package! tree-sitter-langs :pin "213b1c4db852a97480e029453af869ff1a0764e0")
(package! ts-fold :pin "01c9ecaaa89966cdcd250ac37c24a9c9f530b725" :recipe (:host github :repo "emacs-tree-sitter/ts-fold"))

(package! lint-result-mode :recipe (:host github :repo "jgrey4296/misc-modes" :files ("major-modes/lint-result-mode/*.el")))

(package! counsel-gtags)
(package! helm-gtags)
