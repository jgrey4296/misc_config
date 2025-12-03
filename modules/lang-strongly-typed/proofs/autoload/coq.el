;;; coq.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;;###autoload
(defun +jg-coq-save-then-goto-point ()
  (interactive)
  (basic-save-buffer)
  (proof-goto-point)
  )



;;; coq.el ends here
