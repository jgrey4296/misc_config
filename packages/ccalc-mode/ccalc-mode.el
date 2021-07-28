;;; ccalc-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 26, 2021
;; Modified: July 26, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/johngrey/ccalc-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar-local ccalc-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst ccalc-font-lock-keywords
  (list)
  "Highlighting for ccalc-mode"
  )

(define-derived-mode ccalc-mode fundamental-mode
  "ccalc"
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map ccalc-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list ccalc-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'ccalc-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'ccalc-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table ccalc-mode-syntax-table)
  ;;
  (setq major-mode 'ccalc-mode)
  (setq mode-name "ccalc")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\\.ccalc" . ccalc-mode))

(provide 'ccalc-mode)

(provide 'ccalc-mode)
;;; ccalc-mode.el ends here
