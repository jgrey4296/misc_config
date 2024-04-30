;;; format.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-python-cleanup-ensure-newline-before-def ()
  (while (re-search-forward "\\(\n\\)\\(\s*@.+?\n\\)*\s*\\(def\\|class\\)" nil t)
    (goto-char (match-end 1))
    (insert "\n")
    (goto-char (match-end 0))
    )
  )

;;;###autoload
(defun +jg-python-align-dictionaries ()
  ;; TODO
  )

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

;;;###autoload
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
