;;; agendspeak-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 25, 2021
;; Modified: July 25, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/johngrey/agendspeak-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;;-- end header

;;-- imports
(require 'agentspeak-faces)

;;-- end imports

;;-- keymap
(defvar-local agentspeak-mode-map
  (make-sparse-keymap))

;;-- end keymap

;;-- fontlock
(defconst agentspeak-font-lock-keywords
  (rx-let ((w (x) (: x (0+ blank)))
           (g (&rest x) (group x))
           (ln (: punctuation line-end))
           (word+ (group  word-start (+ word) word-end (0+ blank)))
           (basic-ops (: symbol-start (| "@" "+" "!" "<-" "?" "-" "&" ":-" "|&|" "<:") symbol-end))
           (basic-kws  (| "percept" "self" "include" "register_function"))
           (start-syms (in "-" "+" "!" "^" "?"))
           (tokens (| "not" "div" "mod" "begin" "end" "if" "else" "elif" "for" "while"))
           (atom (| (: word-start (| lower (: "." word)) (* (| word "."))) (: "'" (+ (not "'")) "'")))
           (a_num (+ digit))
           (var (| (: upper (* word)) (: "_" (+ digit) (* word) )))
           ;; (logexpr (: "[" (regexp ".+?") "]")) ;; word+ (* "," word+) "]"))
           (logexpr (regexp "\\[.+\\]"))
           )
    (list
     `(,(rx atom)
       (0 font-lock-type-face))
     `(,(rx (| a_num "true" "false"))
       (0 font-lock-constant-face))
     `(,(rx var)
       (0 font-lock-variable-name-face))
     `(,(rx basic-ops)
       (0 'font-lock-operator-face t))
     `(,(rx bol "@" word+ logexpr)
       (0 '(:background "darkslateblue") prepend))
     ;; Plans:
     `(,(rx bol (g (+ start-syms)) word+ (g logexpr))
       (1 'font-lock-operator-face t)
       (2 font-lock-function-name-face t)
       (3 '(:background "darkslategray") prepend t)
       )
     `(,(rx "." eol)
       (0 'error))
     )
    )
  "Highlighting for agentspeak-mode"
)

()
;;-- end fontlock

;;-- syntax
(defvar agentspeak-mode-syntax-table
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
    (modify-syntax-entry ?\" "\"" st)
    ;; Pair parens, brackets, braces
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?: ".:2" st)
    (setq agentspeak-mode-syntax-table st))
  "Syntax table for the agentspeak-mode")

;;-- end syntax

;;-- indent
(defun agentspeak-indent-line ()

  ;; 'noindent
  )

;;-- end indent

(defun agentspeak-mode-beginning-of-defun (count)
  (re-search-backward (rx bol (+ (in "-" "+" "!" "^" "?")) (+ word)) nil t count)
  )

(defun agentspeak-mode-end-of-defun ()
  (re-search-forward (rx "." eol) nil t)
  )

;;-- mode definition
(define-derived-mode agentspeak-mode fundamental-mode
  "agentspeak"
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map agentspeak-mode-map)

  (set (make-local-variable 'font-lock-defaults) (list agentspeak-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'agentspeak-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'agentspeak-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table agentspeak-mode-syntax-table)

  (setq-local indent-line-function 'agentspeak-indent-line
              comment-style '(plain)
              comment-start "//"
              comment-use-syntax t
              outline-regexp (rx (+ (in "-" "+" "!" "^" "?")) (+ word))
              outline-heading-end-regexp (rx "\n")
              outline-blank-line t
              outline-level (lambda () 1)
              beginning-of-defun-function #'agentspeak-mode-beginning-of-defun
              end-of-defun-function #'agentspeak-mode-end-of-defun
              )

  (setq major-mode 'agentspeak-mode)
  (setq mode-name "agentspeak")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)
  )

(add-to-list 'auto-mode-alist '("\\.asl" . agentspeak-mode))

;;-- end mode definition

(provide 'agentspeak-mode)
;;; agendspeak-mode.el ends here
