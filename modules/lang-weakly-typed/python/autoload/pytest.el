;;; pytest.el -*- lexical-binding: t; -*-
(require 'transient)

;;;###autoload (autoload '+jg-python-test-logfile "lang-weakly-typed/python/autoload/pytest.el" nil t)
(transient-define-argument +jg-python-test-logfile ()
  " add logging to file "
  :class 'transient-option
  :description "Logfile"
  :argument "--log-file="
  :allow-empty t
  :reader #'(lambda (prompt &optional initial-input history )
              (format "%s.log" (f-join (f-parent (buffer-file-name)) (f-base (buffer-file-name)))))
  )
