;;; jg-list/+funcs.el --- summary -*- lexical-binding: t -*-

;; Simple Functions to feed into sort-subr
(defun +__jg-lisp-key-start ()
  (re-search-forward "(defun " nil t)
  (symbol-at-point))
(defun +__jg-lisp-next-rec-end-func ()
  (evil-forward-end 'evil-defun))
(defun +__jg-lisp-next-rec-func ()
  (evil-forward-beginning 'evil-defun))

;; Run this
(defun +jg-lisp-sort-defuns ()
  (interactive)
  (goto-char (point-min))
  (sort-subr nil
             #'+__jg-lisp-next-rec-func
             #'+__jg-lisp-next-rec-end-func
             #'+__jg-lisp-key-start)
  (goto-char (point-min))
  )
