;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: April 07, 2023
;; Modified: April 07, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(local-load! "+defs")
(local-load! "+vars")
(local-load! "+extra-configs")

(defer-load! jg-bindings-total "+bindings")

(defer-load! jg-evil-ex-bindings "+evil-ex")

(after! transient-toggles (+jg-ide-extend-toggles))

(advice-add '+eval--ensure-in-repl-buffer    :filter-return #'+jg-repl-fix)
(advice-add '+jg-send-region-to-repl         :filter-args #'+jg-advice-send-repl-auto-line)

(when (modulep! +lsp)
  (advice-add 'lsp-diagnostics-flycheck-enable :around #'+lsp--respect-user-defined-checkers-a)
  (advice-add 'lsp-describe-session            :around #'+jg-lsp-dont-select-session)
  (advice-add 'lsp--shutdown-workspace         :around #'+lsp-defer-server-shutdown-a)
  (advice-add 'lsp--auto-configure             :around #'+lsp--use-hook-instead-a)
  (advice-add 'lsp-diagnostics--flycheck-level :before #'+lsp--log-diagnostic-build)
  (local-load! "+lsp")
  )

(when (modulep! +eglot)
  (advice-add 'eglot--managed-mode             :around #'+lsp--defer-server-shutdown-a)
  (local-load! "+eglot")
  )

(when (modulep! +semantic) (local-load! "+semantic"))

(when (modulep! +flycheck) (local-load! "+flycheck"))

(when (modulep! +treesitter) (local-load! "+treesitter"))

;;; config.el ends here
