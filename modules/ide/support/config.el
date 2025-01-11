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
(local-load! "+extra")

(defer-load! jg-bindings-total "+bindings")

(defer-load! jg-evil-ex-bindings "+evil-ex")

(advice-add '+eval--ensure-in-repl-buffer    :filter-return #'+jg-support-repl-fix-a)
(advice-add '+jg-send-region-to-repl         :filter-args   #'+jg-support-send-repl-auto-line-a)

(when (modulep! +lsp) (local-load! "+lsp"))

(when (modulep! +eglot) (local-load! "+eglot"))

(when (modulep! +semantic) (local-load! "+semantic"))

(when (modulep! +flycheck) (local-load! "+flycheck"))

(when (modulep! +treesitter) (local-load! "+treesitter"))

(when (modulep! +gtags) (load-load! "+gtags"))

;;; config.el ends here
