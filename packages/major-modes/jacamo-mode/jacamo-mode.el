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
;;  used jacamo/doc/jcm.adoc for syntax
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

;; === Syntax for agents section
;; <agents>    ::= <agent>*
;; <agent>     ::= agent <name> [ : <source> ] { <parameter>* }
;; <parameter> ::= <id> : <value> ( (, | EOL) <value> ) *
;; <id>        ::= beliefs | goals | debug | verbose | ag-class | ag-arch | ag-bb-class | myparameter | instances | join | focus | roles
;; === Syntax for environment section
;; <environment> ::= <workspace>*
;; <workspace>   ::= workspace <name> { <artifact>* <agents> <debug> }
;; <artifact>    ::= artifact  <name> : <type> [ { focused-by: [<namespace> ::] <agents> } ]
;; <agents>      ::= agents : <name> ( (, | EOL) <name> )* | "*"   // "*" means all agents
;; <debug>       ::= debug
;; === Syntax for organisations section
;; <organisations> ::= <org>*
;; <org>           ::= organisation <name> [ : <source> ] { <parameter>* <agents> <group>* <scheme>* }
;; <group>         ::= group  <name> : <type> [ { <gparameter>* } ]
;; <gparameter>    ::= <gid> : <gvalue> ( (, | EOL) <gvalue> ) *
;; <gid>           ::= responsible-for | debug | group | players | owner
;; <scheme>        ::= scheme <name> : <type> [ { <sparameter>* } ]
;; <sparameter>    ::= <sid> : <svalue> ( (, | EOL) <svalue> ) *
;; <sid>           ::= debug | owner

(defconst jacamo-font-lock-keywords
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
     `(,(rx line-start (g (w "mas")) word+ (? (* blank) "uses" (group (*? any)) "{"))
       (1 font-lock-keyword-face t)
       (2 font-lock-variable-name-face t))
     ;; <agent>     ::= agent <name> [ : <source> ] { <parameter>* }
     `(,(rx (g (w "agent")) word+ (w ":") word+ )
       (1 font-lock-keyword-face t)
       (2 font-lock-variable-name-face t)
       (3 font-lock-type-face))
     ;; <artifact>    ::= artifact  <name> : <type> [ { focused-by: [<namespace> ::] <agents> } ]
     `(,(rx (g (w (| "scheme" "artifact"))) word+ (w ":"))
       (1 font-lock-keyword-face t)
       (2 font-lock-variable-name-face t))
     ;; <workspace>   ::= workspace <name> { <artifact>* <agents> <debug> }
     `(,(rx (g (w "workspace"))  word+)
       (1 font-lock-keyword-face t)
       (2 font-lock-variable-name-face t))
     ;; <org>           ::= organisation <name> [ : <source> ] { <parameter>* <agents> <group>* <scheme>* }
     `(,(rx (g (w "organisation")) word+ (w ":") word+)
       (1 font-lock-keyword-face t)
       (2 font-lock-variable-name-face t)
       (3 font-lock-type-face t))
     ;; <group>         ::= group  <name> : <type> [ { <gparameter>* } ]
     `(,(rx (g (w "group")) word+ (w ":") word+)
       (1 font-lock-keyword-face t)
       (2 font-lock-variable-name-face t)
       (3 font-lock-type-face t))
     ;; <id>        ::= beliefs | goals | debug | verbose | ag-class | ag-arch | ag-bb-class | myparameter | instances | join | focus | roles
     ;; <gid>           ::= responsible-for | debug | group | players | owner
     `(,(rx (g (| agent-ids org-ids "agents")) ":")
       (1 font-lock-constant-face t))
     )
    )
  "Highlighting for jacamo-mode"
  )

()
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
  "Syntax table for the jacamo-mode")

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
