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


(defun +jg-bindings-clear-buffer ()
  " Utility to clear a buffer
    from https://stackoverflow.com/questions/24565068/ "
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer))
  )
(defun +jg-personal-insert-lparen ()
  " utility to insert a (  "
  (interactive)
  (insert "(")
  )
(defun +jg-personal-insert-rparen ()
  " utility to insert a ) "
  (interactive)
  (insert ")")
  )

(defun +jg-toggle-line-numbers ()
  (interactive)
  (setq display-line-numbers (if (not (eq display-line-numbers t)) t nil))
  )
(defun +jg-toggle-line-numbers-visual ()
  (interactive)
  (setq display-line-numbers (if (not (eq display-line-numbers 'visual)) 'visual nil))
  )
(defun +jg-toggle-window-dedication ()
  (interactive)
  (let ((curr-window (selected-window)))
    (set-window-dedicated-p curr-window
                            (not (window-dedicated-p curr-window)))
    (if (window-dedicated-p curr-window)
        (message "Window is now dedicated to %s" (window-buffer curr-window))
      (message "Window is not dedicated"))
    )
  )
(defun +jg-toggle-line-move-ignore-invisible ()
  (interactive)
  (setq line-move-ignore-invisible (not line-move-ignore-invisible))
  (message "Ignore invisible lines: %s" line-move-ignore-invisible)
  )

(defun +jg-narrow-around-point ()
  (interactive)
  (cond (current-prefix-arg
         (narrow-to-region (line-beginning-position)
                           (point-max)))
        ((eq evil-state 'visual)
         (narrow-to-region evil-visual-beginning evil-visual-end))
        ((not (buffer-narrowed-p))
         (let ((num (read-number "Lines Around Point to Select: ")))
           (narrow-to-region (line-beginning-position (- num))
                             (line-end-position num))
           )
         )
        (t
         (widen))
        )
  )
(defun +jg-toggle-narrow-buffer (arg)
  "Narrow the buffer to BEG END. If narrowed, widen it.
If region isn't active, narrow away anything above point
"
  (interactive "P")
  (cond ((eq evil-state 'normal)
         (narrow-to-region (line-beginning-position) (point-max)))
        ((eq evil-state 'visual)
         (narrow-to-region evil-visual-beginning evil-visual-end))
        )
  )
(defun +jg-narrowing-move-focus-backward (arg)
  (interactive "p")
  (+jg-narrowing-move-focus-forward(- arg))
  )
(defun +jg-narrowing-move-focus-forward (arg)
  (interactive "p")
  (widen)
  (evil-forward-section-begin arg)
  (let ((bounds (+evil:defun-txtobj)))
    (narrow-to-region (car bounds) (cadr bounds))
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

(defun +jg-bindings-insert-debug ()
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet jg-binding-debug-snippet-name) (point)))


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
