;;; pytest.el -*- lexical-binding: t; -*-

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


;;;###autoload
(transient-append-suffix 'python-pytest-dispatch "-c" '("-f" "Logfile" +jg-python-test-logfile))

;;;###autoload
(transient-append-suffix 'python-pytest-dispatch "D" '("q" "Quit" transient-quit-one))

;;;###autoload
(transient-append-suffix 'python-pytest-dispatch "-x" '("--tc" "Trace Config" "--trace-config"))
