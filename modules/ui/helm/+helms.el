;;; +helms.el -*- lexical-binding: t; -*-

;;-- helm actions
(defun +jg-completion-helm-open-random-action (candidate)
  " Helm Action that opens files randomly, by prompting for a file extension
   searching as necessary, and keeping a log of files opened before "
  (let* ((candidates (helm-marked-candidates))
         (file_ext (read-string "File Extension: "))
         (log_file (f-join (if (f-dir? (car candidates)) (car candidates) (f-dirname (car candidates))) ".emacs_rand_file_log"))
         )
    (if (-all-p 'f-file-p candidates)
        ;; if given files, open randomly
        (find-file (seq-random-elt candidates))
      ;; if given directories, search them
      (let ((all_files (-flatten (seq-map (lambda (x) (directory-files-recursively x file_ext)) candidates)))
            (already_used_files (if (f-exists? log_file) (with-temp-buffer
                                                           (insert-file-contents log_file)
                                                           (let ((uf (make-hash-table :test 'equal)))
                                                             (seq-each (lambda (x) (puthash (downcase x) 't uf)) (split-string (buffer-string) "\n"))
                                                             uf))
                                  (progn (message "Making new hash table")
                                         (make-hash-table :test 'equal))))
            (stay_looping 't)
            )
        (while (and stay_looping (> (length all_files) (length (hash-table-keys already_used_files))))
          (let ((the_choice (seq-random-elt all_files)))
            (message "Checking for: %s" the_choice)
            (message "Result: %s" (gethash (downcase the_choice) already_used_files))
            (if (not (gethash (downcase the_choice) already_used_files))
                (progn
                  (write-region the_choice nil log_file 'append)
                  (write-region "\n" nil log_file 'append)
                  (setq stay_looping nil)
                  (find-file the_choice)
                  )
              )
            )
          )
        )
      )
    )
  )
(defun +jg-completion-helm-describe-random-action (candidate)
  "Helm action to describt how many of a directory's files have been randomly opened,
versus not"
  (let* ((candidates (helm-marked-candidates))
         (file_ext (read-string "File Extension: "))
         (log_file (f-join (if (f-dir? (car candidates)) (car candidates) (f-dirname (car candidates))) ".emacs_rand_file_log"))
         (used-files (if (f-exists? log_file) (with-temp-buffer
                                                (insert-file-contents log_file)
                                                (let ((uf (make-hash-table :test 'equal)))
                                                  (seq-each (lambda (x) (puthash (downcase x) 't uf)) (split-string (buffer-string) "\n"))
                                                  uf))
                       (make-hash-table :test 'equal)))
         (all-files (-flatten (seq-map (lambda (x) (directory-files-recursively x file_ext)) candidates)))
         (count 0)
         (unopened '())
         )
    (mapc (lambda (x) (if (not (gethash (downcase x) used-files)) (progn (cl-incf count) (push x unopened)))) all-files)
    (with-current-buffer (messages-buffer)
      (message "%s" (mapconcat 'identity unopened "\n"))
      )
    (message "Files not opened randomly: %s" count)
    )
  )
(defun +jg-completion-bookmark-load-random ()
  " Open a random bookmark, log it, and provide a
      temp buffer to edit tags in "
  (interactive)
  (widen)
  (let* ((location (f-dirname (buffer-file-name)))
         (log_file (f-join location ".emacs_rand_bookmark_log"))
         (log_hash (if (f-exists? log_file) (with-temp-buffer
                                              (insert-file-contents log_file)
                                              (let ((uf (make-hash-table :test 'equal)))
                                                (seq-each (lambda (x) (puthash x 't uf)) (split-string (buffer-string) "\n"))
                                                uf))
                     (make-hash-table :test 'equal)))
         )
    ;; go to random line
    ;;(alist-get 'HREF (cadr data)) = href/tags
    ;;caddr data = name
    (goto-char (random (point-max)))
    (goto-char (line-beginning-position))
    (forward-char 4)
    (let ((entry (xml-parse-tag)))
      (while entry
        (if (gethash (alist-get 'HREF (cadr entry) nil nil 'equal) log_hash)
            (progn (goto-char (random (point-max)))
                   (goto-char (line-beginning-position))
                   (forward-char 4)
                   (setq entry (xml-parse-tag)))
          (progn
            (write-region (alist-get 'HREF (cadr entry) nil nil 'equal)
                          nil log_file 'append)
            (write-region "\n" nil log_file 'append)
            (narrow-to-region (line-beginning-position) (line-end-position))
            (goto-char (point-min))
            (org-open-link-from-string (message "[[%s]]" (alist-get 'HREF (cadr entry))))
            (setq entry nil)
            )
          )
        )
      )
    )
  )
(defun +jg-completion-helm-open-random-external-action (candidate)
  " Open a random file in an external program, optionally specifying wildcard "
  (interactive)
  (let* ((pattern (car (last (f-split candidate))))
         (pattern-r (wildcard-to-regexp pattern))
         (files (helm-get-candidates (helm-get-current-source)))
         (all_matches (if (string-match-p "\*\." pattern)
                          (seq-filter (lambda (x) (string-match-p pattern-r x)) files)
                        (f-files candidate)))
         (selected (seq-random-elt all_matches)))
    (debug)
    ;; TODO (spacemacs//open-in-external-app selected)
    ))
(defun +jg-completion-helm-open-random-exploration-action (candidate)
  " Randomly choose a directory until an openably file is found (wildcard optional)"
  ;; TODO
  (interactive)
  (let* ((pattern (car (last (f-split candidate))))
         (pattern-r (wildcard-to-regexp pattern))
         (files (helm-get-candidates (helm-get-current-source)))
         (all_matches (if (string-match-p "\*\." pattern)
                          (seq-filter
                           (lambda (x) (string-match-p pattern-r x))
                           files)
                        (f-files candidate)))
         (selected (seq-random-elt all_matches)))
    (debug)
    ;; TODO (spacemacs//open-in-external-app selected)
    ))
;;-- end helm actions

;;-- helm transformers
(defun +jg-completion-helm-rps-transformer (x)
  " Cleans a Candidate line for display  "
  (if (string-match "\.com/\\([0-9/]+\\)/have-you-played-\\(.+?\\)/" x)
      `(,(format "%s : %s" (match-string 1 x)
                 (s-replace "-" " " (match-string 2 x)))
        . ,x)
    `(,x . ,x)
    )
  )
;;-- end helm transformers

(defun +jg-completion-switch-major-mode ()
  (interactive)
  (let ((major-modes +jg-personal-major-modes))
    (helm
     :sources '((name . "Major modes")
                (candidates . major-modes)
                (action . (lambda (mode) (funcall (intern mode))))
                (persistent-action . (lambda (mode) (describe-function (intern mode)))))))

  )
(defun +jg-completion-rps-have-you-playeds ()
  (interactive)
  (let* ((target jg-misc-rps-have-you-played-loc)
         (source (helm-build-in-file-source "Have You Played Helm" target
                   :candidate-transformer (lambda (x)
                                            (mapcar #'+jg-completion-helm-rps-transformer x))
                   :action (helm-make-actions "Open" #'(lambda (x) (mapcar #'browse-url (helm-marked-candidates))))
                   )))
    (helm :sources (list source)
          :buffer "*helm have you played*")
    )

  )
(defun +jg-completion-xkcd ()
  " TODO transformers "
  (interactive)
  (let* ((target "/Volumes/documents/github/bibliography/plus/urls/xkcds")
         (source (helm-build-in-file-source "xkcd helm" target
                   :action (helm-make-actions "Open" #'(lambda (x) (mapcar #'browse-url (helm-marked-candidates))))
                   )))
    (helm :sources (list source)
          :buffer "*helm xkcd*")
    )
  )
