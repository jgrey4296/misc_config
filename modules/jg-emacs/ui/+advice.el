;;; +advice.el -*- lexical-binding: t; -*-

(after! doom-ui
  (advice-remove 'kill-current-buffer #'doom--switch-to-fallback-buffer-maybe-a)
  )

(define-advice kill-current-buffer (:before-until (&rest _)
                                      +jg-ui-kill-buffer-override)
    "Switch to `doom-fallback-buffer' if on last real buffer.

Advice for `kill-current-buffer'. If in a dedicated window, delete it. If there
are no real buffers left OR if all remaining buffers are visible in other
windows, switch to `doom-fallback-buffer'. Otherwise, delegate to original
`kill-current-buffer'."
    (let* ((buf (current-buffer))
           (win-list (get-buffer-window-list buf nil t))
           ;; (visible-p (-contains? win-list (selected-window)))
           (inhibit-redisplay t)
           (doom-inhibit-switch-buffer-hooks t)
           buffer-list-update-hook
           )
      (cond ((eq buf (doom-fallback-buffer))
             (message "Can't kill the fallback buffer.")
             t
             )
            ((window-dedicated-p)
             (delete-window)
             t
             )
            ((and (doom-real-buffer-p buf)
                  (not (-contains? win-list (selected-window)))
                  (buffer-modified-p buf)
                  (not (y-or-n-p (format "Buffer %s is modified; kill anyway?" buf))))
             (user-error "Aborted")
             t
             )
            ((not (cl-set-difference (doom-real-buffer-list)
                                     (doom-visible-buffers)))
             (kill-buffer buf)
             (switch-to-buffer (doom-fallback-buffer))
             t
            )
            (t nil)
            )
      )
    )
