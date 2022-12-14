
(defun +jg-completion-counsel-bookmark ()
  "Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist.
Modified to pre-sort bookmarks, caselessly
"
  (interactive)
  (require 'bookmark)
  (ivy-read "Create or jump to bookmark: "
            (cl-sort (bookmark-all-names) #'string-lessp :key #'downcase)
            :history 'bookmark-history
            :action (lambda (x)
                      (cond ((and counsel-bookmark-avoid-dired
                                  (member x (bookmark-all-names))
                                  (file-directory-p (bookmark-location x)))
                             (with-ivy-window
                               (let ((default-directory (bookmark-location x)))
                                 (counsel-find-file))))
                            ((member x (bookmark-all-names))
                             (with-ivy-window
                               (bookmark-jump x)))
                            (t
                             (bookmark-set x))))
            :caller 'counsel-bookmark))

(defun +jg-completion-counsel-features ()
  " Insert from a list of recognized features "
  (interactive)
  (ivy-read "Available Features: "
            (seq-sort #'string-lessp features)
            :require-match nil
            :action 'insert
            )
  )

(defun +jg-completion-counsel-workspace ()
    "Forward to `' or `workspace-set' if workspace doesn't exist."
    (interactive)
    (require 'bookmark)
    (ivy-read "Create or jump to workspace: "
              (+workspace-list-names)
              :history 'workspace-history
              :action (lambda (x)
                        (message "Got: %s" x)
                        (cond ((string-equal x (+workspace-current-name))
                               (message "Eq")
                               (+workspace-save x))
                              (t
                               (+workspace-switch x t))))
              :caller 'counsel-workspace)
    )
