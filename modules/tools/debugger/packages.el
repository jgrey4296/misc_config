;; -*- no-byte-compile: t; -*-
;;; tools/debugger/packages.el

(when (package! realgud)
  (when (modulep! :lang javascript)
    (package! realgud-trepan-ni)))

(when (modulep! +lsp)
  (package! dap-mode)
  (package! posframe))
