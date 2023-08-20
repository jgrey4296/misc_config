;; funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom--recentf-file-truename-fn (file)
  (if (or (not (file-remote-p file))
          (equal "sudo" (file-remote-p file 'method)))
      (abbreviate-file-name (file-truename (tramp-file-name-localname file)))
    file))
