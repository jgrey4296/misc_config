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
(advice-add 'doom--help-package-configs :before-until #'+jg-help-package-config-advice)

;;;###autoload
(defun +jg-help-protect-insert-button (label &optional uri line)
  "short circuits `doom--help-insert-button if passed a null value"
  label
  )

;;;###autoload
(advice-add 'doom--help-insert-button :before-while #'+jg-help-protect-insert-button)
