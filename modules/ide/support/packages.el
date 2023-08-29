;; -*- no-byte-compile: t; -*-
;;; tools/lsp/packages.el

(package! eglot)
(package! flycheck-eglot)

(package! lsp-mode :pin  "a655f3600e040f872408da0e9c1b9fe65ca0aad9")
(package! lsp-ui   :pin  "295d8984da06a745b0a36c56e28ce915bc389adb")
(package! lsp-ivy  :pin  "9ecf4dd9b1207109802bd1882aa621eb1c385106")

(package! cedet :built-in 'prefer)
(package! semantic)

(package! flycheck)
(package! flycheck-popup-tip)
(package! flycheck-posframe)

(package! tree-sitter)
(package! tree-sitter-indent)
(package! tree-sitter-langs)

(package! lint-result-mode :recipe (:host github :repo "jgrey4296/misc-modes" :files ("major-modes/lint-result-mode/*.el")))
