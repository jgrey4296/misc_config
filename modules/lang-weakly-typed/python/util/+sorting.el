;;; jg-list/+funcs.el --- summary -*- lexical-binding: t -*-

;; Simple Functions to feed into sort-subr
(defun +__jg-python-key-start ()
  (re-search-forward "^\s*def ")
  (symbol-at-point))

(defun +__jg-python-next-rec-end-func ()
  (python-nav-forward-sexp))

(defun +__jg-python-next-rec-func ()
  ;; Move Forward
  (beginning-of-defun -1)
  ;; Handle dectorators
  (while (er--python-looking-at-decorator -1)
    (forward-line -1)
    )
  )

;; Run this
(defun +jg-python-sort-class-methods ()
  (interactive)
  (+jg-python-select-class)
  (narrow-to-region evil-visual-beginning evil-visual-end)
  (goto-char (point-min))
  (evil-normal-state)
  ;; narrow to class
  (+__jg-python-next-rec-func)
  (sort-subr nil
             #'+__jg-python-next-rec-func
             #'+__jg-python-next-rec-end-func
             #'+__jg-python-key-start)
  (goto-char (point-min))
  (widen)
  )
