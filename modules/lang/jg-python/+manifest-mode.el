;;; +manifest-mode.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 06, 2022
;; Modified: July 06, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.1))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defvar-local manifest-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst manifest-font-lock-keywords
  (list
   ;; backquote (,(rx ) (subexp facename override laxmatch))
   `(,(rx line-start (or "include"
                        "exclude"
                        "recursive-include"
                        "recusive-exclude"
                        "global-include"
                        "global-exclude"
                        "prune"
                        "graft")) (0 'font-lock-builtin-face))
   )
  "Highlighting for manifest-mode"
  )


(defvar manifest-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?: "." st)
    st)
  "Syntax table for the tag-mode")


(define-derived-mode manifest-mode fundamental-mode
  "manifest"
  (interactive)
  (kill-all-local-variables)
  (use-local-map manifest-mode-map)

  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list manifest-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'manifest-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'manifest-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table manifest-mode-syntax-table)
  ;;
  (setq major-mode 'manifest-mode)
  (setq mode-name "manifest")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("MANIFEST.in" . manifest-mode))

;;; +manifest-mode.el ends here
