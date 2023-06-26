;;; pydoc.el -*- lexical-binding: t; -*-

(defvar jg-python-pydoc-process nil)

;;;###autoload
(defun +jg-python-start-pydoc ()
  "Start pydoc and open the browser"
  (interactive)
  (cond ((not jg-python-pydoc-process)
         (message "Starting Pydoc")
         (setq jg-python-pydoc-process (start-process "pydoc" "*PyDoc*" "python" "-m" "pydoc" "-b")))
        ((process-live-p jg-python-pydoc-process)
         (message "Reopening Browser")
         (process-send-string jg-python-pydoc-process "b\n")
         )
        (t
         (message "Clearing Pydoc")
         (setq jg-python-pydoc-process nil))
        )
    )
