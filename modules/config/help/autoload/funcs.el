;;; funcs.el -*- lexical-binding: t; -*-

(defun jg-help-wk-filter-fn (binding)
  "function to reject bindings when using which-key "
  (not (string-match (rx (or "C-"
                             "C-M"
                             "M-"
                             ;; "s-"
                             (: "<" (+? anychar) ">")
                             ))
                     (car binding)))
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
   nil nil #'jg-help-wk-filter-fn "Top-level bindings")
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

;;;###autoload
(defun +jg-help-report-after-loads ()
  (interactive)
  (when (get-buffer "*After-Loads*") (kill-buffer (get-buffer "*After-Loads*")))
  (with-temp-buffer-window "*After-Loads*"
      'display-buffer-use-some-window
      nil
      (princ (format "There are %s after-loads\nAnd %s after-load functions\n****\n\n" (length after-load-alist) (length after-load-functions)))
    (cl-loop for queued in (sort (mapcar #'(lambda (x)
                                             (cond ((symbolp x) (symbol-name x))
                                                   ((stringp x) x)
                                                   (t (format "%s" x))))
                                         (mapcar #'car after-load-alist)) #'string-lessp)
             do
             (princ (format "- %s\n" queued))
             )
    (princ (format "\n\n****\nThere are %s after-loads\nAnd %s after-load functions" (length after-load-alist) (length after-load-functions)))
    )
  )

;;;###autoload
(defun +jg-help-describe-active-maps ()
  (interactive)
  (let ((maps (current-active-maps)))
    (with-temp-buffer-window "*Maps*" 'display-buffer-use-some-window nil
       (cl-loop for map in maps
                do
                (princ map)
                (princ "\n\n")
                )
       )
    )
  (with-current-buffer "*Maps*"
    (emacs-lisp-mode)
    )
  )
