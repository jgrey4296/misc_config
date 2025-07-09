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
(defun file-notify-rm-watch-silent-advice (descriptor)
  " Removes the callback from a file notification watcher *before* cancelling it "
  (when-let* ((watch (gethash descriptor file-notify-descriptors)))
    (setf (file-notify--watch-callback
           (gethash descriptor file-notify-descriptors)) #'identity))
  )

;;;###autoload
(defun +jg-default-change-ext ()
  "Rename the buffer file to have specified extension"
  (interactive)
  (let* ((current (buffer-file-name))
        (curr-ext (f-ext current))
        (newext  (read-string (format "Extension %s -> ." curr-ext)))
        )
    (message "Converting %s -> %s" current (f-swap-ext current newext))
    (rename-file current (f-swap-ext current newext))
    )
  )
