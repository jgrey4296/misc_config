;;; pdf-meta-mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: November 18, 2022
;; Modified: November 18, 2022
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
;;  to help with adding bookmarks to pdfs
;;
;;; Code:
(require 'evil)
;;-- end header

(defun pdf-meta-beginning-of-section (&optional arg)
  (re-search-backward "^\\w+$" nil t arg)
  )

(defun pdf-meta-end-of-section ()
  (forward-line)
  (re-search-forward "^\\w+$" nil t)
  (beginning-of-line)
  (backward-char)
  )

(defun pdf-meta-inc-bookmark-level (&optional arg)
  (interactive)
  (let ((bounds (if (eq evil-state 'visual)
                    (cons evil-visual-beginning evil-visual-end)
                  (bounds-of-thing-at-point 'defun)))
        )
    (save-excursion
      (goto-char (car bounds))
      (while (re-search-forward "BookmarkLevel: " (cdr bounds) t)
        (evil-numbers/inc-at-pt (if arg -1 1) (point))
        )
      )
    )
  )

(defun pdf-meta-dec-bookmark-level ()
  (interactive)
  (pdf-meta-inc-bookmark-level t)
  )

;;-- key-map
(defvar-local pdf-meta-mode-map
  (make-sparse-keymap))

(evil-make-overriding-map pdf-meta-mode-map)

(evil-define-key 'normal pdf-meta-mode-map ">" #'pdf-meta-inc-bookmark-level)
(evil-define-key 'visual pdf-meta-mode-map ">" #'pdf-meta-inc-bookmark-level)
(evil-define-key 'normal pdf-meta-mode-map "<" #'pdf-meta-dec-bookmark-level)
(evil-define-key 'visual pdf-meta-mode-map "<" #'pdf-meta-dec-bookmark-level)
;;-- end key-map

;;-- font-lock
(defconst pdf-meta-font-lock-keywords
  (list
   ;;backquote (,(rx ) (subexp facename override laxmatch))
   `(,(rx line-start (group-n 1 (* word)) ":" (* blank) (group-n 2 (* any)) line-end)
     (1 font-lock-builtin-face)
     (2 font-lock-variable-name-face)
     )
   )
  "Highlighting for pdf-meta-mode"
  )

;;-- end font-lock

(define-derived-mode pdf-meta-mode fundamental-mode
  "pdf-meta"
  (interactive)
  (kill-all-local-variables)
  (use-local-map pdf-meta-mode-map)

  (setq-local forward-sexp-function nil
              beginning-of-defun-function 'pdf-meta-beginning-of-section
              end-of-defun-function 'pdf-meta-end-of-section
              paragraph-start "\\w+$"
              outline-regexp "\\w+$"
              outline-level '(lambda () 0)
              )
  ;; font-lock-defaults: (keywords disable-syntactic case-fold syntax-alist)
  (set (make-local-variable 'font-lock-defaults) (list pdf-meta-font-lock-keywords nil))
  ;; (set (make-local-variable 'font-lock-syntactic-face-function) 'pdf-meta-syntactic-face-function)
  ;; (set (make-local-variable 'indent-line-function) 'pdf-meta-indent-line)
  ;; (set (make-local-variable 'comment-style) '(plain))
  ;; (set (make-local-variable 'comment-start) "//")
  ;; (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table pdf-meta-mode-syntax-table)
  ;;
  (setq major-mode 'pdf-meta-mode)
  (setq mode-name "pdf-meta")
  (run-mode-hooks)
  (outline-minor-mode)
  (yas-minor-mode)
  )

(add-to-list 'auto-mode-alist '("\.pdf_meta$" . pdf-meta-mode))

(provide 'pdf-meta-mode)
;;; pdf-meta-mode.el ends here
