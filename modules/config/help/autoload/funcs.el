;;; funcs.el -*- lexical-binding: t; -*-

(defvar jg-help-local-var-skip-regexp (rx (or "-map"
                                              "keymap"
                                              "display-table"
                                              "imenu-generic-expression"
                                              "font-lock-keywords"))
  )

;;;###autoload
(defun +jg-help-load-package-list ()
  (unless doom--help-packages-list
    (setq doom--help-packages-list
          (delete-dups
           (append (mapcar #'car package-alist)
                   (mapcar #'car package--builtins)
                   (mapcar #'intern
                           (hash-table-keys straight--build-cache))
                   (mapcar #'car (doom-package-list 'all))
                   nil)))
    )
  )

;;;###autoload
(defun +jg-help-top-level-keymap (&optional _)
  "Show top-level bindings."
  (interactive)
  (which-key--create-buffer-and-show
   nil nil '+jg-bindings-wk-filter-fn "Top-level bindings")
  )

;;;###autoload
(defun +jg-help-buffer-list (curr)
  (cl-remove-if-not #'(lambda (buf)
                        (with-current-buffer buf
                          (and (not (eq curr buf))
                               (derived-mode-p 'helpful-mode))
                          ))
                    (buffer-list))
  )

;;;###autoload
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

;;;###autoload
(defun +jg-help-reset-major-mode ()
  (interactive)
  (fundamental-mode)
  (set-auto-mode)
  (font-lock-debug-fontify)
  )
