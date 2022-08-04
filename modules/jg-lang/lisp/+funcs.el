;;; jg-list/+funcs.el --- summary -*- lexical-binding: t -*-

;; Simple Functions to feed into sort-subr
(defun +jg-lisp-key-start ()
  (re-search-forward "(defun " nil t)
  (symbol-at-point))
(defun +jg-lisp-next-rec-end-func ()
  (evil-forward-end 'evil-defun))
(defun +jg-lisp-next-rec-func ()
  (evil-forward-beginning 'evil-defun))
;; Run this
(defun +jg-lisp-sort-defuns ()
  " A Lisp buffer sorting function "
  (interactive)
  (goto-char (point-min))
  (+jg-lisp-next-rec-func)
  (sort-subr nil
             #'+jg-lisp-next-rec-func
             #'+jg-lisp-next-rec-end-func
             #'+jg-lisp-key-start)
  (goto-char (point-min))
  )
