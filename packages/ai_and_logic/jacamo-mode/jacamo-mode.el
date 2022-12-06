;;; jacamo-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 26, 2021
;; Modified: July 26, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/johngrey/jacamo-mode
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
(require 'evil)
(require 'jacamo-faces)

;;-- end imports

;;-- keymap
(defvar-local jacamo-mode-map
  (make-sparse-keymap))

;;-- end keymap

;;-- font lock
;; List of '(regex (groupnum "face")+)
(defconst jacamo-font-lock-keywords
  (rx-let ((w (x) (: x (0+ blank)))
           (ln (: punctuation line-end))
           (basic-syms (| "@" "+" "!" "<-" "?" "-" "&"))
           (basic-kws  (| "percept" "self" "include" "register_function"))
           )
    (list
     `(,(rx line-start "mas" blank (*? word) (? (* blank) "uses" (group (*? any)) "{"))
       (0 font-lock-keyword-face)
       (1 font-lock-variable-name-face))
     `(,(rx line-start (* blank) (group (or "agent" "goals" "beliefs" "goals" "debug" "verbose"
                                            "instances" "join" "focus" "roles" "workspace" "artifact"
                                            "agents" "organisation" "group" "owner" "players"
                                            (: (or "asl" "org" "java") "-path")
                                            (: "jacamo.platform." (or "Agent" "Environment" "Organisation") "WebInspector")
                                            )))
       (1 font-lock-keyword-face))
     )
    )
  "Highlighting for jacamo-mode"
  )

;;-- end font lock

;;-- syntax
(defvar jacamo-mode-syntax-table
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

;;-- mode definition
(define-derived-mode jacamo-mode prog-mode
  "jacamo"
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map jacamo-mode-map)

  (set (make-local-variable 'font-lock-defaults) (list jacamo-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'jacamo-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'jacamo-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table jacamo-mode-syntax-table)
  ;;
  (setq major-mode 'jacamo-mode)
  (setq mode-name "jacamo")
  (outline-minor-mode)
  (yas-minor-mode)
  (run-mode-hooks)
  )
(add-to-list 'auto-mode-alist '("\\.\\(jcm\\|mas2j\\)$" . jacamo-mode))

;;-- end mode definition


(provide 'jacamo-mode)
;;; jacamo-mode.el ends here
