;;; commands.el -*- lexical-binding: t; -*-

;;; Commands

;;;###autoload
(defun +snippets/goto-start-of-field ()
  "Go to the beginning of the current field."
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
         (active-field (yas--snippet-active-field snippet))
         (position (if (yas--field-p active-field)
                       (yas--field-start active-field)
                     -1)))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

;;;###autoload
(defun +snippets/goto-end-of-field ()
  "Go to the end of the current field."
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
         (active-field (yas--snippet-active-field snippet))
         (position (if (yas--field-p active-field)
                       (yas--field-end active-field)
                     -1)))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

;;;###autoload
(defun +snippets/delete-backward-char (&optional field)
  "Prevents Yas from interfering with backspace deletion."
  (interactive)
  (let ((field (or field (and (overlayp yas--active-field-overlay)
                              (overlay-buffer yas--active-field-overlay)
                              (overlay-get yas--active-field-overlay 'yas--field)))))
    (unless (and (yas--field-p field)
                 (eq (point) (marker-position (yas--field-start field))))
      (call-interactively #'delete-backward-char))))

;;;###autoload
(defun +snippets/delete-forward-char-or-field (&optional field)
  "Delete forward, or skip the current field if it's empty. This is to prevent
buggy behavior when <delete> is pressed in an empty field."
  (interactive)
  (let ((field (or field (and yas--active-field-overlay
                              (overlay-buffer yas--active-field-overlay)
                              (overlay-get yas--active-field-overlay 'yas--field)))))
    (cond ((not (yas--field-p field))
           (delete-char 1))
          ((and (not (yas--field-modified-p field))
                (eq (point) (marker-position (yas--field-start field))))
           (yas--skip-and-clear field)
           (yas-next-field 1))
          ((eq (point) (marker-position (yas--field-end field))) nil)
          ((delete-char 1)))))

;;;###autoload
(defun +snippets/delete-to-start-of-field (&optional field)
  "Delete to start-of-field."
  (interactive)
  (unless field
    (setq field (and (overlayp yas--active-field-overlay)
                     (overlay-buffer yas--active-field-overlay)
                     (overlay-get yas--active-field-overlay 'yas--field))))
  (when (yas--field-p field)
    (let ((sof (marker-position (yas--field-start field))))
      (when (and field (> (point) sof))
        (delete-region sof (point))))))

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
(defun +snippets/new ()
  "Create a new snippet in `+snippets-dir'."
  (interactive)
  (let ((default-directory
          (expand-file-name (symbol-name major-mode)
                            +snippets-dir)))
    (+snippet--ensure-dir default-directory)
    (with-current-buffer (switch-to-buffer "untitled-snippet")
      (snippet-mode)
      (erase-buffer)
      (yas-expand-snippet (concat "# -*- mode: snippet -*-\n"
                                  "# name: $1\n"
                                  "# uuid: $2\n"
                                  "# key: ${3:trigger-key}${4:\n"
                                  "# condition: t}\n"
                                  "# --\n"
                                  "$0"))
      (when (bound-and-true-p evil-local-mode)
        (evil-insert-state)))))

;;;###autoload
(defun +snippets/new-alias (template-uuid)
  "Create an alias for a snippet with uuid TEMPLATE-UUID.

You will be prompted for a snippet to alias."
  (interactive
   (list
    (+snippet--completing-read-uuid "Select snippet to alias: "
                                    current-prefix-arg)))
  (unless (require 'doom-snippets nil t)
    (user-error "This command requires the `doom-snippets' library bundled with Doom Emacs"))
  (let ((default-directory (expand-file-name (symbol-name major-mode) +snippets-dir)))
    (+snippet--ensure-dir default-directory)
    (with-current-buffer (switch-to-buffer "untitled-snippet")
      (snippet-mode)
      (erase-buffer)
      (yas-expand-snippet
       (concat "# -*- mode: snippet -*-\n"
               "# name: $1\n"
               "# key: ${2:trigger-key}${3:\n"
               "# condition: t}\n"
               "# type: command\n"
               "# --\n"
               "(%alias \"${4:" (or template-uuid "uuid") "}\")"))
      (when (bound-and-true-p evil-local-mode)
        (evil-insert-state)))))

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
