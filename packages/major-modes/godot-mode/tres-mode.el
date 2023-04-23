;;; tres-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: April 18, 2023
;; Modified: April 18, 2023
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
;;  For Godot .tres files
;;
;;; Code:

;;-- end header

;;-- imports

;;-- end imports

;;-- keymap

(defvar-local tres-mode-map
  (make-sparse-keymap))

;;-- end keymap

;;-- font lock

(defconst tres-font-lock-keywords
  (rx-let ((w (x) (: x (0+ blank)))
           (g (x) (group x))
           (ln (: punctuation line-end))
           (word+ (group word-start (+ (| word punct)) (0+ blank)))
           (basic-syms (| "@" "+" "!" "<-" "?" "-" "&"))
           (basic-kws  (| "percept" "self" "include" "register_function"))
           (agent-ids (| "beliefs" "goals" "debug" "verbose" "ag-class" "ag-arch" "ag-bb-class" "myparameter" "instances" "join" "focus" "roles"))
           (org-ids   (| "responsible-for" "debug" "group" "players" "owner"))
           )
    (list

     )
    )
  "Highlighting for tres-mode"
  )

;;-- end font lock

;;-- syntax

(defvar tres-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Symbols
    (modify-syntax-entry ?. "_" st)
    (modify-syntax-entry ?! "_" st)
    (modify-syntax-entry ?$ "_" st)
    (modify-syntax-entry ?+ "_" st)
    (modify-syntax-entry ?- "_" st)
    (modify-syntax-entry ?? "_" st)
    (modify-syntax-entry ?@ "_" st)
    (modify-syntax-entry ?\; "_" st)
    ;;underscores are valid parts of words:
    (modify-syntax-entry ?_ "w" st)
    ;; Comments start with // and end on newlines
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* "_ 23b" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Strings
    (modify-syntax-entry ?" """ st)
    ;; Pair parens, brackets, braces
    (modify-syntax-entry ?( "()" st)
                         (modify-syntax-entry ?\[ "(]" st)
                         (modify-syntax-entry ?{ "(}" st)
                         (modify-syntax-entry ?: ".:2" st)
                         st)
    "Syntax table for the tres-mode"
    )

  ;;-- end syntax

;;-- mode definition

  (define-derived-mode tres-mode fundamental-mode
    "tres"
    (interactive)
    (kill-all-local-variables)
    (use-local-map tres-mode-map)

    ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
    (set (make-local-variable 'font-lock-defaults) (list tres-font-lock-keywords nil))
    ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'tres-syntactic-face-function)
    ;; (set (make-local-variable 'indent-line-function) 'tres-indent-line)
    (set (make-local-variable 'comment-style) '(plain))
    (set (make-local-variable 'comment-start) "//")
    (set (make-local-variable 'comment-use-syntax) t)
    (set-syntax-table tres-mode-syntax-table)
    ;;
    (setq major-mode 'tres-mode)
    (setq mode-name "tres")
    (outline-minor-mode)
    (yas-minor-mode)
    (run-mode-hooks)
    )
  (add-to-list 'auto-mode-alist '("\.tres" . tres-mode))

;;-- end mode definition

(provide 'tres-mode)
;;; tres-mode.el ends here
