;;; config/default/autoload/files.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +default/browse-notes ()
  "Browse files from `org-directory'."
  (interactive)
  (unless (bound-and-true-p org-directory)
    (require 'org))
  (doom-project-browse org-directory))

;;;###autoload
(defun +default/find-in-notes ()
  "Find a file under `org-directory', recursively."
  (interactive)
  (unless (bound-and-true-p org-directory)
    (require 'org))
  (doom-project-find-file org-directory))

;;;###autoload
(defun +default/find-file-under-here ()
  "Perform a recursive file search from the current directory."
  (interactive)
  (doom-project-find-file default-directory))

;;;###autoload
(defun +default/discover-projects (arg)
  "Discover projects in `projectile-project-search-path'.
If prefix ARG is non-nil, prompt for the search path."
  (interactive "P")
  (if arg
      (call-interactively #'projectile-discover-projects-in-directory)
    (if (not projectile-project-search-path)
        (user-error "`projectile-project-search-path' is empty; don't know where to search")
      (letf! (defun projectile-add-known-project (project-root)
               (unless (projectile-ignored-project-p project-root)
                 (funcall projectile-add-known-project project-root)
                 (message "Added %S to known project roots" project-root)))
        (dolist (dir projectile-project-search-path)
          (cl-destructuring-bind (dir . depth) (if (consp dir) dir (cons dir nil))
            (if (not (file-accessible-directory-p dir))
                (message "%S was inaccessible and couldn't be searched" dir)
              (projectile-discover-projects-in-directory dir depth))))))))

;;;###autoload
(defun +default/dired (arg)
  "Open a directory in dired.
If prefix ARG is non-nil, prompt for a known project to open in dired."
  (interactive "P")
  (apply #'dired
         (if arg
             (list (completing-read "Open dired in project: " projectile-known-projects))
           (dired-read-dir-and-switches ""))))

;;;###autoload
(defun file-notify-rm-watch-silent-advice (descriptor)
  " Removes the callback from a file notification watcher *before* cancelling it "
  (when-let* ((watch (gethash descriptor file-notify-descriptors)))
    (setf (file-notify--watch-callback
           (gethash descriptor file-notify-descriptors)) #'identity))
  )

;;;###autoload
(advice-add 'file-notify-rm-watch :before #'file-notify-rm-watch-silent-advice)
