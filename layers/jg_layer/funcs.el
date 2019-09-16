;; jg_emacs funcs.el
;; loaded third.

;; (when (configuration-layer/package-usedp 'package)
;;   (defun spacemacs/<package>-enable () )
;;   (defun spacemacs/<package>-disable () ))


;;----------------------------------------
(when (configuration-layer/package-usedp 'auto-complete)
  (defun jg_layer/ac-trigger ()
    (interactive)
    (auto-complete)
    )
  )

;;----------------------------------------
(when (configuration-layer/package-usedp 'dired)

  (defun jg_layer/dired-auto-move ()
    " Function to move up a directory, to the next line, and into that "
    (interactive)
    (dired-up-directory)
    (evil-next-line)
    (dired-find-file)
    (spacemacs/open-file-or-directory-in-external-app nil)
    (let* ((current-dir (dired-current-directory))
           (lst (dired-split "/" current-dir))
           (search-str (apply 'f-join (-take-last 3 lst))))
      (evil-window-right 1)
      (goto-char (point-min))
      (message "Searching for %s" search-str)
      (search-forward search-str)
      (evil-scroll-line-to-center (line-number-at-pos (point)))
      (search-forward "tags = {")
      )
    )
  )

(when (configuration-layer/package-usedp 'evil-quickscope)
  (defun jg_layer/toggle-quickscope-always ()
    (interactive)
    (evil-quickscope-always-mode (if (eq nil evil-quickscope-always-mode)
                                     1
                                   0))
    )
  )

;;--------------------------------------------------
(defun jg_layer/insert-lparen ()
  """ utility to insert a (  """
  (interactive)
  (insert "(")
  )

(defun jg_layer/insert-rparen ()
  """ utility to insert a ) """
  (interactive)
  (insert ")")
  )

(defun jg_layer/flatten (lst)
  """ Utility to flatten a list """
  (letrec ((internal (lambda (x)
                       (cond
                        ((null x) nil)
                        ((atom x) (list x))
                        (t
                         (append (funcall internal (car x)) (funcall internal (cdr x))))))))
    (progn
      (assert (listp lst))
      (funcall internal lst))))

(defun jg_layer/clear-buffer ()
  """ Utility to clear a buffer
    from https://stackoverflow.com/questions/24565068/ """
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer))
  )

(defun jg_layer/caseless_sort_buffer ()
  (interactive)
  (let ((sort-fold-case 't))
    (sort-lines nil (point-min) (point-max))
    )
  )

(defun jg_layer/goto-org-agenda-file ()
  (interactive)
  (let ((agenda (car org-agenda-files)))
    (find-file agenda)
    )
  )

(defun jg_layer/goto-messages ()
  (interactive)
  (switch-to-buffer "*Messages*")
  )

(defun jg_layer/goto-home ()
  (interactive)
  (find-file "~")
  )

(defun jg_layer/goto-resources ()
  (interactive)
  (find-file "~/github/writing/resources")
  )

(defun jg_layer/goto-desktop ()
  (interactive)
  (find-file "~/Desktop")
  )

(defun jg_layer/goto-github ()
  (interactive)
  (find-file "~/github")
  )

(defun jg_layer/goto-mega ()
  (interactive)
  (find-file "~/mega")
  )

(defun jg_layer/line-starts-with? (text)
  (s-starts-with? text (s-trim-left (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))
  )

(defun jg_layer/helm-open-random-action (candidate)
  """ Helm Action that opens files randomly, by prompting for a file extension
   searching as necessary, and keeping a log of files opened before """
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

(defun jg_layer/helm-describe-random-action (candidate)
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
    (mapc (lambda (x) (if (not (gethash (downcase x) used-files)) (progn (incf count) (push x unopened)))) all-files)
    (with-current-buffer (messages-buffer)
      (message "%s" (mapconcat 'identity unopened "\n"))
      )
    (message "Files not opened randomly: %s" count)
    )
  )

(defun jg_layer/bookmark-load-random ()
  """ Open a random bookmark, log it, and provide a
      temp buffer to edit tags in """
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

(defun jg_layer/helm-open-random-external-action (candidate)
    " Open a random file in an external program, optionally specifying wildcard "
    (interactive)
    (let* ((pattern (car (last (f-split candidate))))
           (pattern-r (wildcard-to-regexp pattern))
           (files (helm-get-candidates (helm-get-current-source)))
           (all_matches (if (string-match-p "\*\." pattern)
                            (seq-filter (lambda (x) (string-match-p pattern-r x)) files)
                          (f-files candidate)))
           (selected (seq-random-elt all_matches)))
      (spacemacs//open-in-external-app selected)
      ))

(defun jg_layer/helm-open-random-exploration-action (candidate)
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
      (spacemacs//open-in-external-app selected)
      ))

;;----------------------------------------

(defun jg_layer/example_transient_func_setup ()
  """ Create an example transient state """

  (defun jg_layer/example_transient_func ()
    """ The simple function to call from within the transient state """
    (interactive)
    (print "blah")
    )

  (spacemacs|define-transient-state example_transient
    """ Define the transient state itself """
    :title "an example transient state"
    :doc (concat "
[_q_]^^ Quit         [_h_]^^ Example Func
[_t_]^^ Example Func
")
    :bindings
    ("q" nil :exit t)
    ("t" jg_layer/example_transient_func)
    ("h" jg_layer/example_transient_func :exit t)
    )

  ;; Then register its activation
  (spacemacs/set-leader-keys "." 'spacemacs/example_transient-transient-state/body)
  )

;;----------------------------------------
(defun jg_layer/example-helm-setup ()
  """ An Example Helm Setup """

  (setq-default jg_layer/example_helm-source
                (helm-make-source "Find Image" 'helm-source-ffiles
                  :action '(("action" . (lambda (candidate) (insert candidate))))))

  (defun jg_layer/example_helm-call ()
    (interactive)
    (helm :sources jg_layer/example_helm-source
          :input "./"))
  )

;;----------------------------------------
  (defun jg_layer/toggle-all-defs ()
    (interactive)
    ;; goto start of file
    (let* ((open-or-close 'evil-close-fold)
           (current (point))
           )
      (save-excursion
        (goto-char (point-min))
        (python-nav-forward-defun)
        (while (not (equal current (point)))
          (setq current (point))
          (if (jg_layer/line-starts-with? "def ")
              (funcall open-or-close))
          (python-nav-forward-defun)
          )
        )
      )
    )

  (defun jg_layer/close-class-defs ()
    (interactive )
    (save-excursion
      (let* ((current (point)))
        (python-nav-backward-defun)
        (while (and (not (jg_layer/line-starts-with? "class "))
                    (not (equal current (point))))
          (evil-close-fold)
          (setq current (point))
          (python-nav-backward-defun)
          )
        )
      )
    (save-excursion
      (let* ((current (point)))
        (python-nav-forward-defun)
        (while (and (not (jg_layer/line-starts-with? "class "))
                    (not (equal current (point))))
          (evil-close-fold)
          (setq current (point))
          (python-nav-forward-defun)
          )
        )
      )
    )

  (defun jg_layer/setup-python-mode ()
    (evil-define-key 'normal python-mode-map
      (kbd "z d") 'jg_layer/toggle-all-defs
      (kbd "z C") 'jg_layer/close-class-defs
      ))

