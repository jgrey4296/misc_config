;;; compile.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;;###autoload
(defun +jg-lisp-compile-commands (&optional dir)
  (interactive)
  (let ((curr-file (buffer-file-name))
        (dir (or dir default-directory))
        )
     (list
      (format "eask test buttercup %s -- %s" (f-parent curr-file) (f-filename curr-file))
      (format "eask test buttercup %s" dir)
      "eask test buttercup"
      "eask install-deps --dev"
      "eask info"
      "eask status"
      "eask"
      )
     )
  )


;;; compile.el ends here
