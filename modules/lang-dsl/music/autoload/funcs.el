;; funcs.el -*- mode: elisp; lexical-binding: t; -*-

;;;###autoload
(defun +jg-sclang-clear-and-run-buffer ()
  (interactive)
  (with-current-buffer sclang-post-buffer
    (erase-buffer)
    )
  (sclang-eval-document)
  (display-buffer sclang-post-buffer)
  )
