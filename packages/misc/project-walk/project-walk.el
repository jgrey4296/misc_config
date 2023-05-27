;;; -*- lexical-binding: t; -*-

;; A Simple minor mode to walk through every file in a project

(defvar project-walk-buffer "*Project-Walk*")

(defvar project-walk-root-text "# Root: ")

(defun project-walk-init ()
  (interactive)
  (let* ((current (projectile-project-root))
         (files (projectile-project-files current)))
    (with-current-buffer (get-buffer-create project-walk-buffer)
      (setq-local doom-real-buffer-p t)
      (erase-buffer)
      (insert project-walk-root-text current "\n")
      (mapc (lambda (x) (insert x "\n")) files)
      (goto-char (point-min))
      (forward-line 1)
      )
    )
  )

(defun project-walk-directory-init ()
  (interactive)
  (let* ((current (read-directory-name "Starting Point: "))
         (files (projectile-project-files current)))
    (with-current-buffer (get-buffer-create project-walk-buffer)
      (erase-buffer)
      (insert project-walk-root-text current "\n")
      (mapc (lambda (x) (insert x "\n")) files)
      (goto-char (point-min))
      (forward-line 1)
      )
    )
  )

(defun project-walk-filter-defaults ()
  (interactive)
  (with-current-buffer project-walk-buffer
    (goto-char (point-min))
    (forward-line 1)
    (flush-lines project-walk-filter-default-regexp)
    (goto-char (point-min))
    (forward-line 1)
    (flush-lines project-walk-filter-default-exclusions)
    )
)

(defun project-walk-filter-keep (arg)
  (interactive "MKeep filenames: ")
  (with-current-buffer project-walk-buffer
    (goto-char (point-min))
    (forward-line 1)
    (keep-lines arg)
    )
  )

(defun project-walk-filter (arg)
  (interactive "MRegexp to filter: ")
  (with-current-buffer project-walk-buffer
    (goto-char (point-min))
    (forward-line 1)
    (flush-lines arg)
    )
)

(defun project-walk--root ()
  (with-current-buffer project-walk-buffer
    (goto-char (point-max))
    (when (re-search-backward project-walk-root-text nil t)
      (buffer-substring (+ (point) (length project-walk-root-text))
                        (line-end-position)))))

(defun project-walk--find (file)
  (let ((root (project-walk--root)))
    (cond ((and (s-matches? "^/" file)
                (f-exists? file))
           (find-file file))
          ((and root (f-exists? (f-join root file)))
           (find-file (f-join root file)))
          (t (message "Doesn't exist: %s %s" file root))
          )
    )
  )

(defun project-walk-next ()
  " Go to the next buffer in 'project-walk-buffer
going by the root (signified by (rx bol #))
or by the last visited file (signified by (rx bol *))
 "
  (interactive)
  (cond ((null (get-buffer project-walk-buffer))
         (message "Project Walk Not Started"))
        (t (with-current-buffer project-walk-buffer
             (goto-char (point-max))
             (if (re-search-backward "^* " nil t)
                 (forward-line 1)
               (goto-char (point-min)))
             (insert "* ")
             (cond ((looking-at "/")
                    (find-file (buffer-substring (point) (line-end-position))))
                   (t (project-walk--find (buffer-substring (point) (line-end-position))))
                   ))
           )
        )
  )

(defun project-walk-prev ()
  (interactive)
  (cond ((null (get-buffer project-walk-buffer))
         (message "Project Walk Not Started"))
        (t (with-current-buffer project-walk-buffer
             (goto-char (point-max))
             (when (re-search-backward "^* " nil t)
               (delete-region (point) (+ (point) 2))
               )
             (cond ((looking-at "/") (find-file (buffer-substring (point) (line-end-position))))
                   (t (project-walk--find (buffer-substring (point) (line-end-position))))
             )))
        )
  )

(defun project-walk-remaining ()
  (interactive)
  (cond ((null (get-buffer project-walk-buffer))
         (message "Project Walk Not Started"))
        ((with-current-buffer project-walk-buffer
           (goto-char (point-max))
           (re-search-backward "^\\(*\\|#\\) ")
           (forward-line)
           (not (< (point) (point-max))))
         (message "Project Walk Completed"))
        ;; ((intern-soft "+popup-buffer")
        ;;  (message "Popping")
        ;;  (+popup-buffer (get-buffer project-walk-buffer)))
        (t
         (message "Displaying")
         (display-buffer project-walk-buffer))
        )
  )

(defun project-walk-num ()
  (interactive)
  (cond ((null (get-buffer project-walk-buffer))
         (message "Project Walk Not Started"))
        ((with-current-buffer project-walk-buffer
           (goto-char (point-max))
           (re-search-backward "^\\(*\\|#\\) ")
           (forward-line)
           (not (< (point) (point-max))))
         (message "Project Walk Completed"))
        (t
         (with-current-buffer project-walk-buffer
           (let ((lineno (list (progn (goto-char (point-max))
                                      (re-search-backward "^\\(*\\|#\\) ")
                                      (forward-line 1)
                                      (line-number-at-pos))
                               (progn (goto-char (point-max))
                                      (line-number-at-pos)))))
             (message "Remaining to Walk: %s/%s" (1- (car lineno)) (1- (cadr lineno)))
             )
           )
         )
        )
  )

;;;###autoload
(define-minor-mode project-walk-minor-mode
  " A minor mode to walk through all "
  :lighter "Project-Walk"
  :global t
  (setq-default project-walk-filter-default-exclusions '("__init__.py")
                project-walk-filter-default-regexp (rx line-start ?.)
                )
  (cond (project-walk-minor-mode
         (project-walk-init))
        ((get-buffer project-walk-buffer)
         (kill-some-buffers (list (get-buffer project-walk-buffer))))
        (t nil)
    )
)

(provide 'project-walk)
