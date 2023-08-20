;;; +ivy.el -*- lexical-binding: t; -*-

(defun +jg-ibuffer-ivy-predicate (x)
  ;; return nil for cruft buffers
  (not (string-match jg-ibuffer-ivy-predicate-patterns (car x)))
  )

;;;###autoload
(defun +jg-ibuffer-ivy-buffer ()
  (interactive)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :predicate           #'+jg-ibuffer-ivy-predicate
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action              #'ivy--switch-buffer-action
            :matcher             #'ivy--switch-buffer-matcher
            :sort t
            :caller 'ivy-switch-buffer)
  )
