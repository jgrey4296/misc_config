;;; test_mode.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;;###autoload
(define-minor-mode rust-test-minor-mode
  "For Simple layering on top of python test files"
  :keymap (make-sparse-keymap)
  )

;;;###autoload
(defun maybe-rust-test-minor-mode ()
  (interactive)
  (if (and (f-ext? (buffer-name) "rs")
           (s-matches? "test_" (f-base (buffer-name))))
      (rust-test-minor-mode 1)
      )
  )

;;; test_mode.el ends here
