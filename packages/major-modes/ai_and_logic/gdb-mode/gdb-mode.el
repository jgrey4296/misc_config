;;; gdb-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
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
;;  mode for highlighting din's curse `gdb` files
;;
;;; Code:

;;-- end header

(defvar-local gdb-mode-map

  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst gdb-font-lock-keywords
  (list
   ;;backquote (,(rx ) (subexp facename override laxmatch))
   )
  "Highlighting for gdb-mode"
  )

(defvar gdb-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?$ "_" st)
    ;;underscores are valid parts of words:
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ "<12" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"\"" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?: ".:2" st)
    st)
  "Syntax table for the gdb-mode")



(define-derived-mode gdb-mode fundamental-mode
  "gdb"
  (interactive)
  (kill-all-local-variables)
  (use-local-map gdb-mode-map)

  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list gdb-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'gdb-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'gdb-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table gdb-mode-syntax-table)
  ;;
  (setq major-mode 'gdb-mode)
  (setq mode-name "gdb")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\.gdb$" . gdb-mode))

(provide 'gdb-mode)
;;; gdb-mode.el ends here
