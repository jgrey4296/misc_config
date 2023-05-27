;;; +advice.el -*- lexical-binding: t; -*-

(advice-remove 'kill-current-buffer #'doom--switch-to-fallback-buffer-maybe-a)

;;;###autoload
(defun +jg-ui-kill-buffer-override  (&rest _)
  "Switch to `doom-fallback-buffer' if on last real buffer.

Advice for `kill-current-buffer'. If in a dedicated window, delete it. If there
are no real buffers left OR if all remaining buffers are visible in other
windows, switch to `doom-fallback-buffer'. Otherwise, delegate to original
`kill-current-buffer'.
TODO add fallback to project root
"
  (let* ((buf (current-buffer))
         (buf-mode (buffer-local-value 'major-mode buf))
         (other-windows (delq (selected-window) (get-buffer-window-list buf nil t)))
         (inhibit-redisplay t)
         (doom-inhibit-switch-buffer-hooks t)
         buffer-list-update-hook
         )
    (cond ((eq buf (doom-fallback-buffer))
           (message "Can't kill the fallback buffer.")
           t)
          ((window-dedicated-p) ;; delete dedicated windows too
           (delete-window)
           t)
          (other-windows ;; ignore if other windows have the buffer
           (bury-buffer)
           t)
          ((eq buf-mode 'dired-mode) nil) ;; kill dired buffers
          ((and (doom-real-buffer-p buf)
                (buffer-modified-p buf)
                (not (y-or-n-p (format "JG: Buffer %s is modified; kill anyway?" buf))))
           (user-error "Aborted")
           t
           )
          ((and (projectile-project-root)
                (not (cl-set-difference (doom-real-buffer-list)
                                        (doom-visible-buffers))))
           (kill-buffer buf)
           ;; in a project, go do the project root
           (find-file (projectile-project-root))
           t
           )
          ((not (cl-set-difference (doom-real-buffer-list)
                                   (doom-visible-buffers)))
           (kill-buffer buf)
           ;; No other buffers, go to fallback
           (switch-to-buffer (doom-fallback-buffer))
           t
           )
          (t nil)
          )
    )
  )

;;;###autoload
(advice-add 'kill-current-buffer :before-until #'+jg-ui-kill-buffer-override)

;;;###autoload
(defun +hl-todo-clamp-font-lock-fontify-region-a (fn &rest args)
  "Fix an `args-out-of-range' error in some modes."
  :around #'hl-todo-mode
  (letf! #'font-lock-fontify-region (apply fn args))
  )

;;;###autoload
(advice-add 'hl-todo-mode :around #'+hl-todo-clamp-font-lock-fontify-region-a)

;;;###autoload
(defun +modeline-disable-icon-in-daemon-a ()
  ;; HACK Fix #4102 due to empty all-the-icons return value (caused by
  ;;      `doom--disable-all-the-icons-in-tty-a' advice) in tty daemon frames.
  (when (display-graphic-p)
    (apply fn args))
  )

;;;###autoload
(advice-add 'doom-modeline-propertize-icon :around #'+modeline-disable-icon-in-daemon-a)

;;;###autoload
(defun +modeline--inhibit-modification-hooks-a (fn &rest args)
  (with-silent-modifications (apply fn args))
  )

;;;###autoload
(advice-add 'ws-butler-after-save :around #'+modeline--inhibit-modification-hooks-a)
