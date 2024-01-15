;;; pydoc.el -*- lexical-binding: t; -*-

(defvar jg-python-pydoc-process nil)
(defvar jg-python-pydoc-cmd "pydoc")
(defvar jg-python-pydoc-results "*PyDoc Search*")

;;;###autoload
(defun +jg-python-start-pydoc ()
  "Start pydoc and open the browser"
  (interactive)
  (cond ((not jg-python-pydoc-process)
         (message "Starting Pydoc")
         (setq jg-python-pydoc-process (start-process jg-python-pydoc-cmd "*PyDoc*" jg-python-pydoc-cmd "-b")))
        ((process-live-p jg-python-pydoc-process)
         (message "Reopening Browser")
         (process-send-string jg-python-pydoc-process "b\n")
         )
        (t
         (message "Clearing Pydoc")
         (setq jg-python-pydoc-process nil))
        )
    )

;;;###autoload
(defun +jg-python-pydoc-search ()
  " Call pydoc and display the result "
  (interactive)
  (let* ((buff (get-buffer-create jg-python-pydoc-results))
        (val (read-string "Search Pydoc For: " (current-word)))
        (rel (projectile-make-relative-to-root (list (buffer-file-name))))
        (py-rel (string-replace ".py" "" (string-replace "/" "." (car rel))))
        )
    (with-current-buffer buff
      (erase-buffer)
      (call-process jg-python-pydoc-cmd nil t nil (string-join (list val ":" )
      )
    (display-buffer buff)
    )
  )
