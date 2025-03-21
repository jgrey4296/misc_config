;; advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom--fix-broken-smie-modes-a (fn &optional arg)
    "Some smie modes throw errors when trying to guess their indentation, like
`nim-mode'. This prevents them from leaving Emacs in a broken state."
    (let ((dtrt-indent-run-after-smie dtrt-indent-run-after-smie))
      (letf! ((defun symbol-config--guess (beg end)
                (funcall symbol-config--guess beg (min end 10000)))
              (defun smie-config-guess ()
                (condition-case e (funcall smie-config-guess)
                  (error (setq dtrt-indent-run-after-smie t)
                         (message "[WARNING] Indent detection: %s"
                                  (error-message-string e))
                         (message ""))))) ; warn silently
        (funcall fn arg))))

;;;###autoload
(defun +jg-invert-dashes (orig-fun &rest args)
  (cond ((not (characterp (car args))) (apply orig-fun args))
        ((char-equal (car args) ?-) ?_)
        ((char-equal (car args) ?_) ?-)
        (t (apply orig-fun args))
        )
  )
