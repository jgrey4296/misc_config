;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! eglot)
(package! flycheck-eglot)
(package! lsp-mode)
(package! lsp-ui)
(package! lsp-ivy)

(package! cedet :built-in 'prefer)
(package! semantic)

(package! tree-sitter)
(package! tree-sitter-langs)
(package! tree-sitter-indent)
(when (modulep! :editor evil +everywhere) (package! evil-textobj-tree-sitter))

(package! flycheck)
(package! flycheck-popup-tip)
(when (modulep! +childframe) (package! flycheck-posframe))
