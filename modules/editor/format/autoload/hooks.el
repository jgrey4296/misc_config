;;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +format-enable-on-save-maybe-h ()
  "Enable formatting on save in certain major modes.

This is controlled by `+format-on-save-enabled-modes'."
  (or (cond ((eq major-mode 'fundamental-mode))
            ((string-prefix-p " " (buffer-name)))
            ((and (booleanp +format-on-save-enabled-modes)
                  (not +format-on-save-enabled-modes)))
            ((and (listp +format-on-save-enabled-modes)
                  (if (eq (car +format-on-save-enabled-modes) 'not)
                      (memq major-mode (cdr +format-on-save-enabled-modes))
                    (not (memq major-mode +format-on-save-enabled-modes)))))
            ((not (require 'format-all nil t))))
      (format-all-mode +1)))
