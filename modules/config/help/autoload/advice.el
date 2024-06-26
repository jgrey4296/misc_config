;;; advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-help-package-config-advice (package)
  (let ((default-directory doom-user-dir))
    (mapcar (-partial #'f-join doom-user-dir)
            (split-string
             (cdr (doom-call-process
                   "rg" "--no-heading" "--line-number"
                   (format "%s%s"
                           "(^;;;###package|\\(after!|\\(use-package!)\s+"
                           package)
                   ))
             "\n" t)))
  )

;;;###autoload
(defun +jg-help-protect-insert-button (label &optional uri line)
  "short circuits `doom--help-insert-button if passed a null value"
  label
  )

;;;###autoload
(defun doom--find-function-search-for-symbol-save-excursion-a (fn &rest args)
  "Suppress cursor movement by `find-function-search-for-symbol'.

Addresses an unwanted side-effect in `find-function-search-for-symbol' on Emacs
29 where the cursor is moved to a variable's definition if it's defined in the
current buffer."
  (let (buf pos)
    (letf! (defun find-library-name (library)
             (let ((filename (funcall find-library-name library)))
               (with-current-buffer (find-file-noselect filename)
                 (setq buf (current-buffer)
                       pos (point)))
               filename))
      (prog1 (apply fn args)
        (when (buffer-live-p buf)
          (with-current-buffer buf (goto-char pos))))))
  )

;;;###autoload
(defun doom-use-helpful-a (fn &rest args)
  "Force FN to use helpful instead of the old describe-* commands."
  (letf! ((#'describe-function #'helpful-function)

          (#'describe-variable #'helpful-variable))
    (apply fn args))
  )
