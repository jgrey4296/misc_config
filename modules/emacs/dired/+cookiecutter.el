;;; +cookiecutter.el -*- lexical-binding: t; -*-

(defvar jg-dired-cookie-cmd "cookiecutter")
(defvar jg-dired-cookie-buffer "*CookieCutter*")
(defvar jg-dired-cookie-args '("--no-input" ))

(defun +jg-dired-cookiecutter ()
  (interactive)

  (let ((templates (mapcar #'f-base (f-directories jg-snippets-project-templates-dir)))
        )
    (with-current-buffer (get-buffer-create jg-dired-cookie-buffer)
      (erase-buffer))
    (apply #'call-process jg-dired-cookie-cmd nil (get-buffer-create "*CookieCutter*") nil (append
                                                           jg-dired-cookie-args
                                                           (list (ivy-read "Project Template: " templates :require-match t))
                                                           (list (concat "proj_name=" (read-string "Project Name: ")))
                                                           ))
    )
  (unless (string-empty-p (with-current-buffer (get-buffer jg-dired-cookie-buffer)
                               (s-trim (buffer-string))))
    (+popup-buffer (get-buffer jg-dired-cookie-buffer))
    )
  )
