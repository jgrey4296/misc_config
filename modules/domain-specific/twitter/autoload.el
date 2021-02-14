;;; domain-specific/twitter/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun tramp-register-crypt-file-name-handler ()
  "Add crypt file name handler to `file-name-handler-alist'."
  (when (and tramp-crypt-enabled tramp-crypt-directories)
    (add-to-list 'file-name-handler-alist
	         (cons tramp-file-name-regexp #'tramp-crypt-file-name-handler))
    (put #'tramp-crypt-file-name-handler 'safe-magic t)))
