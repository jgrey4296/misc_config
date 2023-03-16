;;; +funcs.el -*- lexical-binding: t; -*-

(defun +jg-help-evil-interactive-reminder ()
  (interactive)
  (let ((evil-text (with-temp-buffer
                     (insert-file-contents
                      (expand-file-name "straight/repos/evil/evil-types.el" doom-local-dir))
                     (buffer-string)
                     )))
    (with-temp-buffer-window "*Evil Interactive Reminder*"
        'display-buffer-pop-up-window
        nil
      (princ evil-text)
      )
    (with-current-buffer  "*Evil Interactive Reminder*"
      (emacs-lisp-mode)
      )
    nil
    )
  )

(defun +jg-help-system-config ()
  (interactive)
  (with-temp-buffer-window "*Emacs Build Configuration*" 'display-buffer-pop-up-window nil
    (princ "Emacs Built with: \n")
    (princ system-configuration-features)
    (cl-loop for line in (s-split " -" system-configuration-options)
             do
             (princ "\n-")
             (princ line)
             )
    )
  )

(defun +jg-help-top-level-keymap (&optional _)
  "Show top-level bindings."
  (interactive)
  (which-key--create-buffer-and-show
   nil nil '+jg-bindings-wk-filter-fn "Top-level bindings")
  )

(defun +jg-help-buffer-list (curr)
  (cl-remove-if-not #'(lambda (buf)
                        (with-current-buffer buf
                          (and (not (eq curr buf))
                               (derived-mode-p 'helpful-mode))
                          ))
                    (buffer-list))
  )

(defun +jg-help-switch-to-prev-helpful-or-close-window ()
  (interactive)
  (if-let ((next-helpful (car-safe (+jg-help-buffer-list (current-buffer))))
           (curr (current-buffer))
           )
      (progn (switch-to-buffer next-helpful t t)
             (kill-buffer curr))
    (+popup/quit-window)
    )
  )
