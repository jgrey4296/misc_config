;;; cargo-makefile-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: December 03, 2022
;; Modified: December 03, 2022
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

;;-- end header

;;-- requires
(require 'conf-mode)

;;-- end requires

;;-- keymap
(defvar-local cargo-makefile-mode-map
  (make-sparse-keymap))

;;-- end keymap

;;-- font-lock
;; List of '(regex (groupnum "face")+)
(defconst cargo-makefile-font-lock-keywords
  '(
    (conf-toml-recognize-section
     0 'font-lock-type-face prepend)
    ("^\\s-*\\(.+?\\)\\(?:\\[\\(.*?\\)\\]\\)?\\s-*="
     (1 'font-lock-variable-name-face)
     (2 'font-lock-constant-face nil t))
    ("\\_<false\\|true\\_>" 0 'font-lock-keyword-face)
    ("^\\[\\(tasks\\|flow\\)\\." 1 'font-lock-keyword-face t)
    ("^\\[\\(env\\)\\]$" 1 'font-lock-keyword-face t)
    )


  "Highlighting for cargo-makefile-mode"
  )

;;-- end font-lock

;;-- syntax
(defvar cargo-makefile-mode-syntax-table
  (let ((st (copy-syntax-table conf-toml-mode-syntax-table)))

    st)
  "Syntax table for the cargo-makefile-mode")

;;-- end syntax

(define-derived-mode cargo-makefile-mode conf-toml-mode
  "cargo-makefile"
  (interactive)
  (kill-all-local-variables)
  (use-local-map cargo-makefile-mode-map)

  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list cargo-makefile-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'cargo-makefile-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'cargo-makefile-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table cargo-makefile-mode-syntax-table)
  ;;
  (setq major-mode 'cargo-makefile-mode)
  (setq mode-name "cargo-makefile")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )

(provide 'cargo-makefile-mode)
;;; cargo-makefile-mode.el ends here
