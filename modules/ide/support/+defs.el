;; +defs.el -*- lexical-binding: t; -*-

(defvar +lsp-defer-shutdown 10 "If non-nil, defer shutdown of LSP servers for this many seconds after last workspace buffer is closed.")

(defvar +lsp--default-read-process-output-max nil)

(defvar +lsp--default-gcmh-high-cons-threshold nil)

(defvar +lsp--optimization-init-p nil)

(defvar +lsp--deferred-shutdown-timer nil)

(defvar +tree-sitter-hl-enabled-modes '(not web-mode typescript-tsx-mode)
  "A list of major modes which should be highlighted by tree-sitter.

If this list begins with `not', then it negates the list.
If it is t, it is enabled in all modes.
If nil, it is disabled in all modes")
