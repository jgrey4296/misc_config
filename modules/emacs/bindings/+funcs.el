;;; util/bindings/+funcs.el -*- lexical-binding: t; -*-

(defun +jg-browse-url (&optional url)
  (interactive)
  (let ((url (cond (url url)
                   ((eq evil-state 'visual)
                    (buffer-substring-no-properties evil-visual-beginning evil-visual-end))
                   (t
                    (read-string "Search For: "))
                   ))
        )
    (cond ((f-exists? url)
           (shell-command (format "open %s" url)))
          ((s-prefix? "@" url)
           (browse-url (format "https://twitter.com/%s" url)))
          ((s-prefix? "http" url)
           (browse-url url))
          ((s-prefix? "www." url)
           (browse-url (format "https://%s" url)))
          ((string-match "\\.com\\|\\.uk" url)
           (browse-url (format "https://%s" url)))
          (t
           (message "Browsing for: %s" (format jg-google-url url))
           (browse-url (format jg-google-url url)))
          )
    )
  )
(defun +jg-bindings-open-link ()
  (interactive)
  (cond ((eq evil-state 'visual)
         (let ((str (buffer-substring-no-properties evil-visual-beginning evil-visual-end)))
           (org-open-link-from-string (format "[[%s]]" (string-trim str)))
           ))
        (t (org-open-at-point 'in-emacs))
        )
  )
(defun +jg-bindings-open-link-externally ()
  (interactive)
  (let ((current-prefix-arg '(16))
        (str (if (eq evil-state 'visual) (buffer-substring-no-properties evil-visual-beginning evil-visual-end) nil))
        )
    (cond ((eq evil-state 'visual)
           (funcall-interactively 'org-open-link-from-string (format "[[%s]]" (string-trim str))))
          ((eq 'link (org-element-type (org-element-context)))
           (call-interactively 'org-open-at-point))
          (t
           (funcall-interactively 'org-open-link-from-string
                                  (format "[[%s]]" (string-trim (buffer-substring-no-properties (line-beginning-position)
                                                                                                (line-end-position))))))
          )
    )
  )

(defun +jg-counsel-workspace ()
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

(defun +jg-bindings-list-buffer-locals ()
  (interactive)
  (let ((vars (buffer-local-variables))
        (buf (buffer-name (current-buffer)))
        )
    (with-temp-buffer-window (format "*Buffer Locals: %s" buf)
        'display-buffer-pop-up-window
        (lambda (wind val) (with-selected-window wind
                        (emacs-lisp-mode))
          val)
      (cl-loop for x in vars do
               (if (string-match jg-binding-local-var-skip-regexp
                                 (symbol-name (car x)))
                   (princ (format "(%s : Skipped)" (car x)))
                 (princ x))
               (princ "\n")
               )
      )
    )
  )

(defun +jg-bindings-wk-filter-fn (binding)
  (not (string-match (rx (or "C-"
                             "C-M"
                             "M-"
                             ;; "s-"
                             ))
                     (car binding)))
  )

(defun +jg-which-key-show-top-level (&optional _)
  "Show top-level bindings."
  (interactive)
  (which-key--create-buffer-and-show
   nil nil '+jg-bindings-wk-filter-fn "Top-level bindings"))
