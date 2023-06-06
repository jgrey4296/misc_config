;;; funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-buffer-has-long-lines-p ()
  (unless (bound-and-true-p visual-line-mode)
    (so-long-detected-long-line-p)))
