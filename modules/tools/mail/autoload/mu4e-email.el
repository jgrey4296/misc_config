;;; email/mu4e/autoload/email.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +mu4e/attach-files (&optional files-to-attach)
  "When called in a dired buffer, ask for a message to attach the marked files to.
When called in a mu4e:compose or org-msg buffer, `read-file-name'to either
attach a file, or select a folder to open dired in and select file attachments
(using `dired-mu4e-attach-ctrl-c-ctrl-c').

When otherwise called, open a dired buffer and enable `dired-mu4e-attach-ctrl-c-ctrl-c'."
  ;; TODO add ability to attach files (+dirs) as a single (named) archive
  (interactive "p")
  (+mu4e-compose-org-msg-handle-toggle (/= 1 files-to-attach))
  (pcase major-mode
    ((or 'mu4e-compose-mode 'org-msg-edit-mode)
     (let ((mail-buffer (current-buffer))
           (location (read-file-name "Attach: " nil nil t "")))
       (if (not (file-directory-p location))
           (pcase major-mode
             ('mu4e-compose-mode
              (save-excursion
                (goto-char (point-max))
                (unless (eq (current-column) 0)
                  (insert "\n\n")
                  (forward-line 2))
                (mail-add-attachment location)))
             ('org-msg-edit-mode (org-msg-attach-attach location)))
         (split-window-sensibly)
         (with-current-buffer (dired location)
           (setq-local dired-mail-buffer mail-buffer)
           (dired-mu4e-attach-ctrl-c-ctrl-c 1)))))
    ('dired-mode
     (unless (and files-to-attach (/= 1 files-to-attach))
       (setq files-to-attach
             (delq nil
                   (mapcar
                    ;; don't attach directories
                    (lambda (f) (if (file-directory-p f) nil f))
                    (nreverse (dired-map-over-marks (dired-get-filename) nil))))))
     (if (not files-to-attach)
         (progn
           (message "No files marked, aborting.")
           (kill-buffer-and-window))
       (if-let ((mail-target-buffer (bound-and-true-p dired-mail-buffer)))
           (progn (kill-buffer-and-window)
                  (switch-to-buffer mail-target-buffer))
         (if (and (+mu4e-current-buffers)
                  (y-or-n-p "Attach files to existing mail composition buffer? "))
             (progn (setf mail-target-buffer
                          (completing-read "Message: " (+mu4e-current-buffers)))
                    (kill-buffer-and-window)
                    (switch-to-buffer mail-target-buffer))
           (kill-buffer-and-window)
           (mu4e-compose 'new)))
       (mapcar
        (pcase major-mode
          ('mu4e-compose-mode #'mail-add-attachment)
          ('org-msg-edit-mode #'org-msg-attach-attach))
        files-to-attach)))
    (_
     (split-window-sensibly)
     (with-current-buffer (call-interactively #'find-file)
       (dired-mu4e-attach-ctrl-c-ctrl-c 1)))))

(define-minor-mode dired-mu4e-attach-ctrl-c-ctrl-c
  "Adds C-c C-c as a keybinding to attach files to a message."
  :lighter "attach"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") '+mu4e/attach-files)
            map)
  (setq header-line-format
        (when dired-mu4e-attach-ctrl-c-ctrl-c
          (substitute-command-keys
           "Mu4e attach active. `\\[+mu4e/attach-files]' to attach the marked files."))))

(defun +mu4e-current-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (or (and (derived-mode-p 'message-mode)
                       (null message-sent-message-via))
                  (eq major-mode 'org-msg-edit-mode))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

;;; Hooks

;;;###autoload
(defun +mu4e-set-from-address-h ()
  "If the user defines multiple `+mu4e-personal-addresses' for email aliases
within a context, set `user-mail-address' to an alias found in the 'To' or
'From' headers of the parent message if present, or prompt the user for a
preferred alias"
  (when-let ((addresses (mu4e-personal-addresses)))
    (setq user-mail-address
          (if mu4e-compose-parent-message
              (if (version<= "1.8" mu4e-mu-version)
                  (let ((to (mu4e-message-field mu4e-compose-parent-message :to))
                        (cc (mu4e-message-field mu4e-compose-parent-message :cc))
                        (from (mu4e-message-field mu4e-compose-parent-message :from)))
                    (or (car (cl-intersection
                              (mapcar (lambda (adr) (plist-get adr :email))
                                      (append to from cc))
                              addresses
                              :test #'equal))
                        (completing-read "From: " addresses)))
                (let ((to (mapcar #'cdr (mu4e-message-field mu4e-compose-parent-message :to)))
                      (cc (mapcar #'cdr (mu4e-message-field mu4e-compose-parent-message :cc)))
                      (from (mapcar #'cdr (mu4e-message-field mu4e-compose-parent-message :from))))
                  (or (car (cl-intersection (append to from cc) addresses
                                            :test #'equal))
                      (completing-read "From: " addresses))))
            (completing-read "From: " addresses)))))
