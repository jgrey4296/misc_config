;;; abl-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 28, 2021
;; Modified: July 28, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/johngrey/abl-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar-local abl-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst abl-font-lock-keywords
  (list)
  "Highlighting for abl-mode"
  )

(define-derived-mode abl-mode fundamental-mode
  "abl"
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map abl-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list abl-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'abl-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'abl-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table abl-mode-syntax-table)
  ;;
  (setq major-mode 'abl-mode)
  (setq mode-name "abl")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\\.abl" . abl-mode))

(provide 'abl-mode)
;;; abl-mode.el ends here
