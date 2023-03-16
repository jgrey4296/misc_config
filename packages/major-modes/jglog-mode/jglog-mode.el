;;; jglog-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: March 16, 2023
;; Modified: March 16, 2023
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

(defvar-local jglog-mode-map
  (make-sparse-keymap))

;; Fontlock:
;; List of '(regex (groupnum "face")+)
(rx-let (

         )

  (defconst jglog-font-lock-keywords
    '(
      ("^\\([A-Z]+\\)\s+:\s+\\(.+?\\)$" (2 'font-lock-type-face))
      ("^\\(DEBUG\\)\s+:\s+\\(.+?\\)$"   (1 'font-lock-comment-face t))
      ("^\\(INFO\\)\s+:\s+\\(.+?\\)$"    (1 'font-lock-type-face t))
      ("^\\(WARNING\\)\s+:\s+\\(.+?\\)$" (0 'font-lock-warning-face t))
      ("^\\(ERROR\\)\s+:\s+\\(.+?\\)$"   (0 'error t))
      ("---+.+"   (0 'header-line-highlight t))
      (" :|: .+?$" (0 'shadow t))
      )
    "Highlighting for jglog-mode"
    )
  )

(defvar jglog-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?. "." st)
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?$ "_" st)
    (modify-syntax-entry ?- "." st)
    ;;underscores are valid parts of words:
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ "<12" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"\"" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?: ".:2" st)
    st)
  "Syntax table for the jglog-mode")

(defun jglog-mode-filter ()
  " choose a log level, create a new buffer with just that level int it "
  (interactive)
  (let* ((level (ivy-read "Log Level: " '(debug info warning error)))
         (regex (format "^%s\s+:" (upcase level)))
         (current (buffer-file-name))
         (filtered (get-buffer-create (format "*Log: %s(filtered)*" (f-filename current))))
         (display-buffer-alist nil)
         (kill-whole-line t)
        )
    (with-current-buffer filtered (erase-buffer))
    (call-process "ggrep" nil filtered nil "-E" regex current)
    (with-current-buffer filtered
      (jglog-mode)
      (goto-char (point-min))
      )
    (display-buffer filtered '(display-buffer-same-window))
    )
  )


(define-derived-mode jglog-mode fundamental-mode
  "jglog"
  (interactive)
  (kill-all-local-variables)
  (use-local-map jglog-mode-map)

  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list jglog-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'jglog-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'jglog-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table jglog-mode-syntax-table)
  ;;
  (setq major-mode 'jglog-mode)
  (setq mode-name "jglog")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)
  (align-regexp (point-min) (point-max) "\\(\s-*\\):")
  (set-buffer-modified-p nil)

  )
(add-to-list 'auto-mode-alist '("log\\..+$" . jglog-mode))

(provide 'jglog-mode)
;;; jglog-mode.el ends here
