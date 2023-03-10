;;; versu-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 26, 2021
;; Modified: July 26, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/johngrey/versu-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'versu-faces)

(defvar-local versu-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst versu-font-lock-keywords
  (list)
  "Highlighting for versu-mode"
  )

(defvar versu-mode-syntax-table
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
  "Syntax table for the versu-mode")



(define-derived-mode versu-mode fundamental-mode
  "versu"
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map versu-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list versu-font-lock-keywords nil))
  (set (make-local-variable 'font-lock-syntactic-face-function) 'versu-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'versu-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table versu-mode-syntax-table)
  ;;
  (setq major-mode 'versu-mode)
  (setq mode-name "versu")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\\.praxis$" . versu-mode))
(add-to-list 'auto-mode-alist '("\\.type$" . versu-mode))
(add-to-list 'auto-mode-alist '("\\.data$" . versu-mode))

(provide 'versu-mode)
;;; versu-mode.el ends here
