;;; instal-mode.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 26, 2021
;; Modified: July 26, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/johngrey/instal-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defvar-local instal-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst instal-font-lock-keywords
  (list)
  "Highlighting for instal-mode"
  )

(define-derived-mode instal-mode fundamental-mode
  "instal"
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map instal-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list instal-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'instal-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'instal-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table instal-mode-syntax-table)
  ;;
  (setq major-mode 'instal-mode)
  (setq mode-name "instal")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\\.ia[[:alpha:]]$" . instal-mode))

(provide 'instal-mode)

(provide 'instal-mode)
;;; instal-mode.el ends here
