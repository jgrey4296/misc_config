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
(package! treesit-fold)

(package! tree-sitter)
(package! tree-sitter-indent)
(package! tree-sitter-langs)
(package! ts-fold)


(package! counsel-gtags)
(package! helm-gtags)

(package! asdf-vm)
