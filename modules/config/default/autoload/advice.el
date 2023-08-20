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

;;;###autoload
(defun doom--recenter-on-load-saveplace-a (&rest _)
  "Recenter on cursor when loading a saved place."
  (if buffer-file-name (ignore-errors (recenter))))

;;;###autoload
(defun doom--inhibit-saveplace-in-long-files-a (fn &rest args)
  (unless doom-large-file-p
    (apply fn args)))

;;;###autoload
(defun doom--dont-prettify-saveplace-cache-a (fn)
  "`save-place-alist-to-file' uses `pp' to prettify the contents of its cache.
`pp' can be expensive for longer lists, and there's no reason to prettify cache
files, so this replace calls to `pp' with the much faster `prin1'."
  (letf! ((#'pp #'prin1)) (funcall fn)))

;;;###autoload
(advice-add 'save-place-find-file-hook :after-while #'doom--recenter-on-load-saveplace-a)

;;;###autoload
(advice-add 'save-place-to-alist :around #'doom--inhibit-saveplace-in-long-files-a)

;;;###autoload
(advice-add 'save-place-alist-to-file :around #'doom--dont-prettify-saveplace-cache-a)
