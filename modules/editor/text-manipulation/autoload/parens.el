;; parens.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +default-open-doc-comments-block (&rest _ignored)
  ;; Expand C-style comment blocks.
  (save-excursion
    (newline)
    (indent-according-to-mode))
  )
