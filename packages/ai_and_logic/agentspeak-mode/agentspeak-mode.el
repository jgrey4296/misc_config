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
           (ln (: punctuation line-end))
           (basic-syms (| "@" "+" "!" "<-" "?" "-" "&"))
           (basic-kws  (| "percept" "self" "include" "register_function"))
           )
    (list
     `(,(rx (| basic-syms (: word-start basic-kws word-end)))
       (0 'font-lock-keyword-face))
     `(,(rx (group-n 1 (1+ (w word))) (? "(" (group-n 2 (opt word (0+ (: (w ",") word)))) ")"))
       (1 'font-lock-function-name-face)
       (2 'font-lock-type-face nil t))
     `(,(rx word-start (group upper (0+ (syntax word))) word-end)
       (0 'font-lock-variable-name-face t)
       )
     `(,(rx "\"" (+? any) "\"")
       (0 'font-lock-string-face t)
       )
     ;; TODO achievement and maintenance goals,
     ;; TODO assert, retract
     )
    )
  "Highlighting for agentspeak-mode"
  )

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

(defun agentspeak-indent-line ()

  ;; 'noindent
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
  (set (make-local-variable 'indent-line-function) 'agentspeak-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table agentspeak-mode-syntax-table)
  ;;
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
