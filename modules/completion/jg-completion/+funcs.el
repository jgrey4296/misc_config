(defun +jg-counsel-bookmark ()
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


(defun +jg-completion-complete-or-snippet (&optional arg)
  (interactive "p")
  (if (not (yas-expand-from-trigger-key))
    (progn
      (message "Company")
      (company-complete-common-or-cycle)
      )
    )
  )

(defun +jg-new-snippet()
  "Create a new snippet in `+snippets-dir'."
  (interactive)
  (let ((default-directory
          (expand-file-name (symbol-name major-mode)
                            +snippets-dir)))
    (+jg-snippet--ensure-dir default-directory)
    (with-current-buffer (switch-to-buffer "untitled-snippet")
      (snippet-mode)
      (erase-buffer)
      (+file-templates--expand t :mode 'snippet-mode)
      (when (bound-and-true-p evil-local-mode)
        (evil-insert-state)))))


(defun +jg-snippet--ensure-dir (dir)
  (unless (file-directory-p dir)
    (if (y-or-n-p (format "%S doesn't exist. Create it?" (abbreviate-file-name dir)))
        (make-directory dir t)
      (error "%S doesn't exist" (abbreviate-file-name dir)))))


(define-advice yas--read-table (:override ()
                                +jg-snippet-read-table)
  (let ((tables (hash-table-keys yas--tables)))
    (intern-soft (ivy-read "Snippet Table: " tables))
    )
  )
