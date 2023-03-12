;;; autoload.el -*- lexical-binding: t; -*-

(message "Loading the autoload file")

;;;###autoload
(defun jg-test-autoload ()
  (interactive)
  (message "This was an autoload")
  )
