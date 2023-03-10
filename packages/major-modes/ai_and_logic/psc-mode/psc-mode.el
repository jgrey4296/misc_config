;;; psc-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: November 17, 2022
;; Modified: November 17, 2022
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
;;  Simple Major Mode for Bethesda PSC script files
;;
;;; Code:

;;-- end header

(defvar-local psc-mode-map

  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst psc-font-lock-keywords
  (list
   ;;backquote (,(rx ) (subexp facename override laxmatch))

   ;; control flow
   `(,(rx (or (: word-start (? (| "e" "E") "nd") (| "If" "if" "Else" "elseif" "while" "return" "Return" "else") word-end)
              (syntax symbol)
              )
          )
     (0 font-lock-constant-face t))

   ;;Keywords:
   `(,(rx
       (group-n 1 line-start (or "Weapon" "ObjectReference" "Ammo" "keyword" "GlobalVariable" "Message" "float" "Bool" "bool" "Form" "idle") word-end (+ blank))
       (group-n 2 word-start  (| "p" "P") "roperty" (+ blank))
       (group-n 3 (? word-start upper (+ word)))
       (group-n 4 (? (+ blank) (| "a" "A") "uto")))
     (1 font-lock-type-face)
     (2 font-lock-keyword-face)
     (3 font-lock-variable-name-face)
     (4 font-lock-keyword-face nil t)
     )
   ;; assignment
   `(,(rx
       (group-n 1 (?? line-start (+ word) (+ blank)))
       (group-n 2 word-start (+ (syntax word))) (+ blank ) "=")
     (1 font-lock-type-face)
     (2 font-lock-variable-name-face)
     )

   ;; alt-commands
   `(,(rx (group-n 1 (+ (syntax word)))
          (? (syntax punctuation)
             (group-n 2 word-start (+ word)))
          (group-n 3 (syntax open-parenthesis)
                   (*? any)
                   (syntax close-parenthesis)))
     (1 font-lock-warning-face nil)
     (2 font-lock-function-name-face nil t)
     (3 font-lock-constant-face nil t)
     )
   ;; blocks
   `(,(rx line-start (* blank) (opt (+ word) (+ blank))
          (group-n 1 (? (| "e" "E") "nd") (| "Function" "Event" "Quest" "scriptName" "Scriptname" "Form" "State" "state" "Property" "property" "function" "event" "EVENT" "import") word-end)
          (group-n 2 (? (+ blank) word-start (+ word)))
       )
     (1 font-lock-keyword-face t)
     (2 font-lock-function-name-face t t)
     )
   ))


   ;; ;; commands?
   ;; `(,(rx (group-n 1 (: word-start (or "native" "return" "GotoState" "Auto" "launch" "disable" "Return" "reload") )))
   ;;   (1 font-lock-warning-face))
   ;; ;; types
   ;; `(,(rx word-start (or "None" "bool" "int" "Weapon" "float" "Bool" "ObjectReference" "string" "self" "True" "False") word-end)
   ;;   (0 font-lock-type-face)
   ;;   )
  ;;  )
  ;; "Highlighting for psc-mode"
  ;; )

(defconst psc-mode-syntax-table (make-syntax-table))

(modify-syntax-entry ?!  "_" psc-mode-syntax-table)
(modify-syntax-entry ?\( "(" psc-mode-syntax-table)
(modify-syntax-entry ?\) ")" psc-mode-syntax-table)
(modify-syntax-entry ?\{ "<" psc-mode-syntax-table)
(modify-syntax-entry ?\} ">" psc-mode-syntax-table)
(modify-syntax-entry ?\; "<" psc-mode-syntax-table)
(modify-syntax-entry ?\n ">" psc-mode-syntax-table)

(define-derived-mode psc-mode fundamental-mode
  "psc"
  (interactive)
  (kill-all-local-variables)
  (use-local-map psc-mode-map)

  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list psc-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'psc-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'psc-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table psc-mode-syntax-table)
  ;;
  (setq major-mode 'psc-mode)
  (setq mode-name "psc")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\.psc$" . psc-mode))

(provide 'psc-mode)
;;; psc-mode.el ends here
