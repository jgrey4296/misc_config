;; +defs.el -*- lexical-binding: t; -*-

(defvar +lsp-defer-shutdown 10 "If non-nil, defer shutdown of LSP servers for this many seconds after last workspace buffer is closed.")

(defvar +lsp--default-read-process-output-max nil)

(defvar +lsp--default-gcmh-high-cons-threshold nil)

(defvar +lsp--optimization-init-p nil)

(defvar +lsp--deferred-shutdown-timer nil)
