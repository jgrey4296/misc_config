;;-- counsel
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
;;-- end counsel

;;-- snippets
(defun +jg-completion-complete-or-snippet (&optional arg)
  (interactive "p")
  (if (not (yas-expand-from-trigger-key))
    (progn
      (message "Company")
      (company-complete-common-or-cycle)
      )
    )
  )

(defun +jg-completion-new-snippet()
  "Create a new snippet in `+snippets-dir'."
  (interactive)
  (let ((default-directory
          (expand-file-name (symbol-name major-mode)
                            +snippets-dir)))
    (+jg-completion-snippet--ensure-dir default-directory)
    (with-current-buffer (switch-to-buffer "untitled-snippet")
      (snippet-mode)
      (erase-buffer)
      (+file-templates--expand t :mode 'snippet-mode)
      (when (bound-and-true-p evil-local-mode)
        (evil-insert-state)))))

(defun +jg-completion-snippet--ensure-dir (dir)
  (unless (file-directory-p dir)
    (if (y-or-n-p (format "%S doesn't exist. Create it?" (abbreviate-file-name dir)))
        (make-directory dir t)
      (error "%S doesn't exist" (abbreviate-file-name dir)))))

(define-advice yas--read-table (:override ()
                                +jg-completion-snippet-read-table)
  (let ((tables (hash-table-keys yas--tables)))
    (intern-soft (ivy-read "Snippet Table: " tables))
    )
  )
;;-- end snippets

;;-- file-templates control
(defun +jg-completion-add-file-templates (sym rules &optional override)
  (cl-assert (hash-table-p jg-completion-file-template-rules))
  (unless (or (gethash sym jg-completion-file-template-rules) (not override))
    (puthash sym
             (cl-loop for (head . body) in rules
                      for priority = (* -1 (or (plist-get body :priority) 0))
                      for clean    = (cl-loop for (k v) on body by #'cddr
                                              unless (eq k :priority)
                                              collect k and collect v)
                      collect (cons priority (cons head clean))
                      )
             jg-completion-file-template-rules)
    )
  )

(defun +jg-completion-activate-file-templates (&optional force)

  (message "Activating File Templates: %s" (hash-table-keys jg-completion-file-template-rules))
  (unless (or jg-completion-file-templates-flat (not force))
    (let ((all-rules (copy-sequence (-flatten-n 1 (hash-table-values jg-completion-file-template-rules)))))
      (setq jg-completion-file-templates-flat
            (mapcar #'cdr (sort all-rules #'(lambda (x y) (< (car x) (car y))))))
      )
    )

  (when jg-completion-file-templates-flat
    (setq +file-templates-dir jg-completion-file-templates-dir
          yas-snippet-dirs (list +snippets-dir
                                 +file-templates-dir
                                 doom-snippets-dir
                                 yasnippet-snippets-dir)
          yas--default-user-snippets-dir (car yas-snippet-dirs)
          +file-templates-alist jg-completion-file-templates-flat
          )
    )
  )
;;-- end file-templates control
