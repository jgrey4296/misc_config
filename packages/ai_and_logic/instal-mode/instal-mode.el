;;; instal-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
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
(require 'instal-faces)

(defvar-local instal-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst instal-font-lock-keywords
  (list
   `(,(rx (: "%%"  (*? anychar) line-end)) (0 'font-lock-comment-face))
   `(,(rx (: word-start (or "institution" "bridge" "source" "sink") word-end))
     (0 'instal-keywords))
   `(,(rx (: word-start (or "type" "if" "not") word-end))
     (0 'instal-keywords))
   `(,(rx (: word-start (or "<=" ">=" "<>" "!=" "<" ">" "=") word-end))
     (0 'instal-operators))
   `(,(rx (: word-start upper (zero-or-more word) word-end))
     (0 'instal-vars))

   ;; Fluents
   `(,(rx (: word-start (or "fluent") word-end))
     (0 'instal-fluents2))
   `(,(rx (: word-start (or "when" "initially") word-end))
     (0 'instal-operators))
   `(,(rx (: word-start (or "cross" "obligation" "transient" "obl" "x" ) word-end))
     (0 '(instal-fluents2 (:weight bold :background "#303030"))))
   `(,(rx (: word-start (opt "x") (or  "initiates" "terminates") word-end))
     (0 'instal-operators))
   `(,(rx (: word-start (or "pow" "perm") word-end))
     (0 'instal-fluents))

   ;; Events
   `(,(rx (: word-start (or "event" "observed" "occurred") word-end))
     (0 'instal-events2))
   `(,(rx (: word-start (or "exogenous" "exo" "external") word-end))
     (0 '(instal-events (:weight bold))))
   `(,(rx (: word-start (or "institutional" "violation" "inst" "viol") word-end))
     (0 'instal-events2))
   `(,(rx (: word-start (opt "x")  "generates"  word-end))
     (0 'instal-operators))
   )
  "Highlighting for instal-mode"
  )

(define-derived-mode instal-mode fundamental-mode
  "instal"
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map instal-mode-map)

  (set (make-local-variable 'font-lock-defaults) (list instal-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'instal-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'instal-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "%%")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'font-lock-extra-managed-props) '(:weight))
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
;;; instal-mode.el ends here
