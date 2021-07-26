;;; jacamo-mode.el -*- lexical-binding: t; -*-
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

(defvar-local jacamo-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst jacamo-font-lock-keywords
  (list)
  "Highlighting for jacamo-mode"
  )

(define-derived-mode jacamo-mode fundamental-mode
  "jacamo"
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map jacamo-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list jacamo-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'jacamo-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'jacamo-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table jacamo-mode-syntax-table)
  ;;
  (setq major-mode 'jacamo-mode)
  (setq mode-name "jacamo")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\\.jcm" . jacamo-mode))

(provide 'jacamo-mode)


(provide 'jacamo-mode)
;;; jacamo-mode.el ends here
