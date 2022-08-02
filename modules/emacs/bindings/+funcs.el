;;; util/bindings/+funcs.el -*- lexical-binding: t; -*-

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

(defun +jg-bindings-evil-interactive-reminder ()
  (interactive)
  (let ((evil-text (with-temp-buffer
                     (insert-file-contents "/Volumes/documents/github/doom-emacs/.local/straight/repos/evil/evil-types.el")
                     (buffer-string)
                     )))
    (with-temp-buffer-window "*Evil Interactive Reminder*"
        'display-buffer-pop-up-window
        nil
      (princ evil-text)
      )
    nil
    )
  )
