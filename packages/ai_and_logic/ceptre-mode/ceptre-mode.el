;;; ceptre-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 25, 2021
;; Modified: July 25, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/johngrey/ceptre-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar-local ceptre-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst ceptre-font-lock-keywords
  (list)
  "Highlighting for ceptre-mode"
  )

(define-derived-mode ceptre-mode fundamental-mode
  "ceptre"
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map ceptre-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list ceptre-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'ceptre-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'ceptre-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table ceptre-mode-syntax-table)
  ;;
  (setq major-mode 'ceptre-mode)
  (setq mode-name "ceptre")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\\.cep" . ceptre-mode))

(provide 'ceptre-mode)
;;; ceptre-mode.el ends here
