;;; infin-d-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: November 09, 2022
;; Modified: November 09, 2022
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
;; Mode for infinity engine `.d` files,
;; as exported from weidu
;;
;;; Code:

;;-- end header

(defvar-local infin-d-mode-map

  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst infin-d-font-lock-keywords
  (list
   ;;backquote (,(rx ) (subexp facename override laxmatch))
   )
  "Highlighting for infin-d-mode"
  )

(define-derived-mode infin-d-mode fundamental-mode
  "infin-d"
  (interactive)
  (kill-all-local-variables)
  (use-local-map infin-d-mode-map)

  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  ;; (set (make-local-variable 'font-lock-defaults) (list infin-d-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'infin-d-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'infin-d-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table infin-d-mode-syntax-table)
  ;;
  (setq major-mode 'infin-d-mode)
  (setq mode-name "infin-d")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\.infin-d" . infin-d-mode))

(provide 'infin-d-mode)
;;; infin-d-mode.el ends here
