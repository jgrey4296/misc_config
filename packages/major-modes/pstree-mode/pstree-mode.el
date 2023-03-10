;;; pstree-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: March 08, 2023
;; Modified: March 08, 2023
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
;;  Use pstree to back an ivy for process control
;;
;;; Code:

;;-- end header

(defvar-local pstree-mode-map
  (make-sparse-keymap))
(defvar pstree-mode-cmd "pstree" "The exe to run for pstree")
(defvar pstree-mode-args "" "args to pass to pstree")

;; Fontlock:
;; List of '(regex (groupnum "face")+)
(rx-let ()
  (defconst pstree-font-lock-keywords
    "Highlighting for pstree-mode"
    )
  )

(define-derived-mode pstree-mode fundamental-mode
  "pstree"
  (interactive)
  (kill-all-local-variables)
  (use-local-map pstree-mode-map)

  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list pstree-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'pstree-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'pstree-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table pstree-mode-syntax-table)
  ;;
  (setq major-mode 'pstree-mode)
  (setq mode-name "pstree")
  (run-mode-hooks)
  ;; (outline-minor-mode)
  ;; (yas-minor-mode)

  ;; Run pstree
  ;; process results
  ;; put into buffer
  ;; display buffer
  )

(defun pstree-mode-call ()
  ;; TODO
  )

(provide 'pstree-mode)
;;; pstree-mode.el ends here
