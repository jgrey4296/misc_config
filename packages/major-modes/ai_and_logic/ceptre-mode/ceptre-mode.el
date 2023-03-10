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
(require 'ceptre-faces)

(defvar-local ceptre-mode-map
  (make-sparse-keymap))

;; List of '(regex (groupnum "face")+)
(defconst ceptre-font-lock-keywords
  (list
   `(,(rx line-start (regexp "#.+") line-end)
     (0 'ceptre-face-3))
   `(,(rx line-start (group-n 1 (+ (+ word) (+ blank)))
          (group-n 2 ?:) (+ blank)
          (group-n 3 (+ alnum))
          ?. line-end)
     (1 'ceptre-face-1)
     (2 'ceptre-face-2)
     (3 'ceptre-face-3))
   `(,(rx line-start (group-n 1 (or "context") "stage") (+ blank)
          (group-n 2 (+ word)) (* blank) ?= (* blank) ?{)
     (1 'ceptre-face-3)
     (2 'ceptre-face-2)
     )
   `(,(rx blank ?- ?o blank)
     (0 'ceptre-face-2)
     )
   `(,(rx (syntax symbol))
     (0 'ceptre-face-3)
     )
   )
  "Highlighting for ceptre-mode"
  )

(defvar ceptre-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?$ "_" st)
    (modify-syntax-entry ?- "_" st)
    (modify-syntax-entry ?* "_" st)
    ;;underscores are valid parts of words:
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?% "<12" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"\"" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?: "_" st)
    (setq ceptre-mode-syntax-table st))
  "Syntax table for the ceptre-mode")

(setq ceptre-prettify-symbols-alist
  '(("-o" . "-o")

    )
  )


(define-derived-mode ceptre-mode fundamental-mode
  "ceptre"
  ""
  (interactive)
  (kill-all-local-variables)
  (use-local-map ceptre-mode-map)

  (set (make-local-variable 'font-lock-defaults) (list ceptre-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'ceptre-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'ceptre-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table ceptre-mode-syntax-table)
  ;; (set (make-local-variable 'prettify-symbols-alist) ceptre-prettify-symbols-alist)
  ;;
  (setq major-mode 'ceptre-mode)
  (setq mode-name "ceptre")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)
  ;; (prettify-symbols-mode)
  )
(add-to-list 'auto-mode-alist '("\\.cep" . ceptre-mode))

(provide 'ceptre-mode)
;;; ceptre-mode.el ends here
