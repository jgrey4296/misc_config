;;; dired.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-python-try-site-packages ()
  (interactive)
  (-if-let* ((curr default-directory)
               (lib (f-join curr "lib"))
               (lib-exists (f-exists? lib))
               (py-dirs (f-directories lib #'(lambda (x) (s-contains? "python3" x))))
               (site-packages (mapcar (-rpartial #'f-join "site-packages") py-dirs))
               (exist-sites (-filter #'f-exists? site-packages))
               )
      (find-file (car exist-sites))
    (message "Couldn't find a site-packages directory")
    )
  )
