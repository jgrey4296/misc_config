;;; palette-mode.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 John Grey
;;
;; Author: John Grey <https://github.com/johngrey>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 26, 2021
;; Modified: July 26, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/johngrey/palette-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defvar-local palette-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst palette-font-lock-keywords
  (list)
  "Highlighting for palette-mode"
  )

(defconst palette-mode-syntax-table (copy-syntax-table emacs-lisp-mode-syntax-table))

(define-derived-mode palette-mode fundamental-mode
  "palette"
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map palette-mode-map)

  ;; (set (make-local-variable 'font-lock-defaults) (list palette-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'palette-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'palette-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table palette-mode-syntax-table)
  ;;
  (setq major-mode 'palette-mode)
  (setq mode-name "palette")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)
  (rainbow-mode)

  )
(add-to-list 'auto-mode-alist '("\\.palette" . palette-mode))


(provide 'palette-mode)
;;; palette-mode.el ends here
