;;; witcher-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
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
;;  simple major mode for witcher3 ws files
;;
;;; Code:

;;-- end header

(defvar-local witcher-mode-map

  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst witcher-font-lock-keywords
  (list
   ;;backquote (,(rx ) (subexp facename override laxmatch))

   ;; class dec
   `(,(rx line-start
          (? "import" (+ blank))
          (? "abstract" (+ blank))
          (? (group-n 1 "statemachine") (+ blank))
          (| "struct" "enum" "class") (+ blank)
          (group-n 2 (+ word))
          (? (+ blank) "extends" (+ blank) (group-n 3 (+ word))))

     (0 font-lock-keyword-face keep)
     (1 font-lock-type-face t t)
     (2 font-lock-type-face t t)
     (3 font-lock-type-face t t)
     )
   ;; var dec
   `(,(rx (* (| "import" "public" "private" "protected") (+ blank))
          (* (| "saved" "editable") (+ blank))
          "var" (+ blank)
          (group-n 1 (+ word) (* (group "," (+ blank) (+ word))))
          (* any)
          ";" )
     (0 font-lock-keyword-face t)
     (1 font-lock-variable-name-face t)
     )
   ;; fun dec
   `(,(rx (? (| "public" "private" "protected") (+ blank))
          (| "event" "function") (+ blank)
          (group-n 1 (+ word))
          (? (syntax open-parenthesis)
             (group-n 2 (*? any))
             (syntax close-parenthesis))
          )
     (0 font-lock-keyword-face)
     (1 font-lock-type-face t)
     (2 font-lock-variable-name-face t t)
     )
   ;; states
   `(,(rx line-start "state" (+ blank)
          (group-n 1 (+ word) (+ blank))
          "in" (+ blank)
          (group-n 2 (+ word)))
     (0 font-lock-keyword-face)
     (1 font-lock-type-face t)
     (2 font-lock-type-face t)
     )
   ;; control
   `(,(rx (| "if" "for" "while")
          (*? blank)
          (syntax open-parenthesis)
          (group-n 1 (*? any))
          (syntax close-parenthesis))
     (0 font-lock-keyword-face)
     (1 font-lock-variable-name-face t t)
     )
   ;; assignment
   `(,(rx (? (| "hint" "default") (+ blank))
          (group-n 1 (+ word)) (+ blank)
          "=" (+ blank)
          (group-n 2 (+? any) ) ";")
     (0 font-lock-keyword-face)
     (1 font-lock-variable-name-face t)
     (2 font-lock-builtin-face t)
     )
   ;; func call
   `(,(rx (group-n 2 (? (+ word) "."))
          (+ word) "("
          (group-n 1 (*? any))
          (| "\)"
             line-end))
     (0 font-lock-function-name-face keep)
     (1 font-lock-variable-name-face prepend t)
     (2 font-lock-variable-name-face t t)
     )
   ;; typing
   `(,(rx (| (: ":" (* blank) (group-n 1 (+ word) (? "<" (+ any) ">")))
             (: "[" (*? any) "]")
             ))
     (0 font-lock-keyword-face t)
     (1 font-lock-type-face t)
     )
   ;; returns
   `(,(rx "return" (+ blank) (group-n 1 (+ word)) (syntax punctuation))
     (0 font-lock-keyword-face t)
     (1 font-lock-builtin-face t)
     )
   )
  "Highlighting for witcher-mode"
  )


(defconst witcher-mode-syntax-table (make-syntax-table))

(modify-syntax-entry ?!  "_" witcher-mode-syntax-table)
(modify-syntax-entry ?\( "(" witcher-mode-syntax-table)
(modify-syntax-entry ?\) ")" witcher-mode-syntax-table)
(modify-syntax-entry ?\{ "(" witcher-mode-syntax-table)
(modify-syntax-entry ?\} ")" witcher-mode-syntax-table)
(modify-syntax-entry ?\/ "<1" witcher-mode-syntax-table)
(modify-syntax-entry ?\* "<2" witcher-mode-syntax-table)
(modify-syntax-entry ?\n ">" witcher-mode-syntax-table)
(modify-syntax-entry ?_  "w" witcher-mode-syntax-table)

(define-derived-mode witcher-mode fundamental-mode
  "witcher"
  (interactive)
  (kill-all-local-variables)
  (use-local-map witcher-mode-map)

  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list witcher-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'witcher-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'witcher-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table witcher-mode-syntax-table)
  ;;
  (setq major-mode 'witcher-mode)
  (setq mode-name "witcher")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\.ws" . witcher-mode))

(provide 'witcher-mode)
;;; witcher-mode.el ends here
