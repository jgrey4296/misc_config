;;; lint-result-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: January 03, 2023
;; Modified: January 03, 2023
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
;;
;;
;;; Code:

;;-- end header

(defvar-local lint-result-mode-map

  (make-sparse-keymap))


;; List of '(regex (groupnum "face")+)
(setq lint-result-mode-font-lock-keywords
      (rx-let ((w (x) (: x (0+ blank)))
               (ln (: punctuation line-end))
               (basic-syms (| "@" "+" "!" "<-" "?" "-" "&"))
               (basic-kws  (| "percept" "self" "include" "register_function"))
               )
        (list
         ;; Header
         `(,(rx line-start (group (w (+ "*"))) (group (+ (w (+ graph))) line-end))
           (1 font-lock-doc-face)
           (2 font-lock-constant-face))

         `(,(rx line-start (group (+? graph)) ":"
                (group (+ digit)) ":" (group (+ digit)) ":" space
                (group alpha digit digit digit digit) ":" space
                (+? any)
                "(" (group (+ (or alpha "-"))) ")" line-end
                )
           (1 font-lock-keyword-face)
           (2 font-lock-variable-name-face)
           (3 font-lock-variable-name-face)
           (4 font-lock-warning-face)
           (5 font-lock-function-name-face)
           )
         )
        )
      )
      ;; "Highlighting for lint-result-mode"
      ;; )

(define-derived-mode lint-result-mode fundamental-mode
  "lint-result-mode"
  (interactive)
  (kill-all-local-variables)
  (use-local-map lint-result-mode-map)

  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list lint-result-mode-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'lint-result-mode-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'lint-result-mode-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table lint-result-mode-syntax-table)
  ;;
  (setq major-mode 'lint-result-mode)
  (setq mode-name "lint-result-mode")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)

  )
(add-to-list 'auto-mode-alist '("\.lint" . lint-result-mode))

(provide 'lint-result-mode)
;;; lint-result-mode.el ends here
