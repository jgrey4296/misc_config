;;; paradox-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
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
;;  simple mode for paradox script files
;;
;;; Code:

;;-- end header

(defvar-local paradox-mode-map

  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst paradox-font-lock-keywords
  (list
   ;;backquote (,(rx ) (subexp facename override laxmatch))

   ;; main assignment
   `(,(rx line-start (+ blank)
          (group-n 1 (+ word) (* "_" (+ word)))
          (+ blank) "=" (+ blank)
          (or (group-n 2 "{")
              (group-n 3 (* any))
              )
          )
     (0 font-lock-builtin-face)
     (1 font-lock-variable-name-face t)
     (3 font-lock-constant-face t t)
     )
   `(,(rx (or "OR" "AND" "NOT"))
     (0 font-lock-constant-face t)
     )
   )
  "Highlighting for paradox-mode"
  )

(defconst paradox-mode-syntax-table (make-syntax-table))

(modify-syntax-entry ?!  "_" paradox-mode-syntax-table)
(modify-syntax-entry ?\( "(" paradox-mode-syntax-table)
(modify-syntax-entry ?\) ")" paradox-mode-syntax-table)
(modify-syntax-entry ?# "<" paradox-mode-syntax-table)
(modify-syntax-entry ?\n ">" paradox-mode-syntax-table)

(define-derived-mode paradox-mode fundamental-mode
  "paradox"
  (interactive)
  (kill-all-local-variables)
  (use-local-map paradox-mode-map)

  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list paradox-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'paradox-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'paradox-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table paradox-mode-syntax-table)
  ;;
  (setq major-mode 'paradox-mode)
  (setq mode-name "paradox")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\.paradox" . paradox-mode))

(provide 'paradox-mode)
;;; paradox-mode.el ends here
