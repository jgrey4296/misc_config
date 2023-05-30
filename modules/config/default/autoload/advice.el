;;; advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +default--newline-indent-and-continue-comments-a (&rest _)
  "A replacement for `newline-and-indent'.
Continues comments if executed from a commented line. Consults
`doom-point-in-comment-functions' to determine if in a comment."
  :before-until #'newline-and-indent
  (interactive "*")
  (when (and +default-want-RET-continue-comments
             (doom-point-in-comment-p)
             (functionp comment-line-break-function))
    (funcall comment-line-break-function nil)
    t))

;;;###autoload
(advice-add 'newline-and-indent :before-until #'+default--newline-indent-and-continue-comments-a)
