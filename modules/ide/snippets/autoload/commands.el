;;; commands.el -*- lexical-binding: t; -*-

;;; Commands

;;;###autoload
(defun +snippets/find ()
  "Open a snippet file (in all of `yas-snippet-dirs')."
  (interactive)
  (let* ((dirs (doom-files-in (cl-loop for dir in yas-snippet-dirs
                                       if (symbolp dir)
                                       collect (symbol-value dir)
                                       else collect dir)
                              :depth 0 :type 'dirs))
         (files (doom-files-in dirs :depth 0 :full t)))
    (let ((template-path (completing-read "Find snippet: " (mapcar #'abbreviate-file-name files))))
      (unless (file-readable-p template-path)
        (user-error "Cannot read %S" template-path))
      (find-file template-path)
      (unless (file-in-directory-p template-path +snippets-dir)
        (read-only-mode +1)
        (setq header-line-format "This is a built-in snippet. Press C-c C-e to modify it"
              +snippet--current-snippet-uuid template-uuid)))))

;;;###autoload
(defun +snippets/find-private ()
  "Open a private snippet file in `+snippets-dir'."
  (interactive)
  (doom-project-find-file +snippets-dir))

;;;###autoload
(defun +snippets/find-for-current-mode (template-uuid)
  "Open a snippet for this mode."
  (interactive
   (list
    (+snippet--completing-read-uuid "Visit snippet: " current-prefix-arg)))
  (if-let* ((template (+snippet--get-template-by-uuid template-uuid major-mode))
            (template-path (yas--template-load-file template)))
      (progn
        (unless (file-readable-p template-path)
          (user-error "Cannot read %S" template-path))
        (find-file template-path)
        (unless (file-in-directory-p template-path +snippets-dir)
          (read-only-mode +1)
          (setq header-line-format "This is a built-in snippet. Press C-c C-e to modify it"
                +snippet--current-snippet-uuid template-uuid)))
    (user-error "Cannot find template with UUID %S" template-uuid)))

;;;###autoload
(defun +snippets/edit (template-uuid)
  "Edit a snippet with uuid TEMPLATE-UUID.

If the snippet isn't in `+snippets-dir', it will be copied there (where it will
shadow the default snippet)."
  (interactive
   (list
    (+snippet--completing-read-uuid "Select snippet to edit: "
                                    current-prefix-arg)))
  (if-let* ((major-mode (if (eq major-mode 'snippet-mode)
                            (intern (file-name-base (directory-file-name default-directory)))
                          major-mode))
            (template (+snippet--get-template-by-uuid template-uuid major-mode))
            (template-path (yas--template-load-file template)))
      (if (file-in-directory-p template-path +snippets-dir)
          (find-file template-path)
        (let ((buf (get-buffer-create (format "*%s*" (file-name-nondirectory template-path)))))
          (with-current-buffer (switch-to-buffer buf)
            (insert-file-contents template-path)
            (snippet-mode)
            (setq default-directory
                  (expand-file-name (file-name-nondirectory template-path)
                                    (expand-file-name (symbol-name major-mode)
                                                      +snippets-dir))))))
    (user-error "Couldn't find a snippet with uuid %S" template-uuid)))

;;;###autoload
(defun +snippets-show-hints-in-header-line-h ()
  (setq header-line-format
        (substitute-command-keys
         (concat "\\[yas-load-snippet-buffer-and-close] to finish, "
                 "\\[+snippet--abort] to abort, "
                 "\\[yas-tryout-snippet] to test it"))))

(defvar +snippets-random-var-names '(bar bloo baz barry other))
(defvar +snippets-random-var-join ":")

;;;###autoload
(defun +snippets-random-var (arg)
  (interactive "P")
  (cond ((consp arg)
         (insert (seq-random-elt +snippets-random-var-names)))
        ((numberp arg)
         (let ((head (seq-random-elt +snippets-random-var-names))
               (body (mapcar #'(lambda (x)
                                 (symbol-name (seq-random-elt +snippets-random-var-names)))
                             (make-list (1- arg) t)))
               )
           (insert ?\"
                   (string-join `(,(symbol-name head) ""
                                      ,@body)
                                +snippets-random-var-join)
                   ?\"
                   )))
        (t
         (insert ?\" (symbol-name (seq-random-elt +snippets-random-var-names))  ?\"))
        )
  )


(defvar jg-snippets-debug-snippet-name "util.debug")

;;;###autoload
(defun +jg-snippets-insert-debug ()
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet jg-snippets-debug-snippet-name) (point))
  )
