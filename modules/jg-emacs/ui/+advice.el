;;; +advice.el -*- lexical-binding: t; -*-

(advice-remove 'kill-current-buffer #'doom--switch-to-fallback-buffer-maybe-a)

(define-advice kill-current-buffer (:before-until (&rest _)
                                                  +jg-ui-kill-buffer-override)
  "Switch to `doom-fallback-buffer' if on last real buffer.

Advice for `kill-current-buffer'. If in a dedicated window, delete it. If there
are no real buffers left OR if all remaining buffers are visible in other
windows, switch to `doom-fallback-buffer'. Otherwise, delegate to original
`kill-current-buffer'."
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
