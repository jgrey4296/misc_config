;;; +advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun jg-ui-kill-current-buffer-logic-a  (&rest _)
  "Advice for `kill-current-buffer'.
Returns t to block killing the buffer

The logic is to:
- bury the buffer if viewable in other windows
- ask if the buffer is modified
- if no more real buffers, switch to the fallback buffer
- otherwise kill


If in a dedicated window, delete it. If there
are no real buffers left OR if all remaining buffers are visible in other
windows, switch to `doom-fallback-buffer'. Otherwise, delegate to original
`kill-current-buffer'.
"
  (let* ((buf (current-buffer))
         (buf-mode (with-current-buffer buf major-mode))
         (in-other-windows (delq (selected-window) (get-buffer-window-list buf nil t)))
         (inhibit-redisplay t)
         (doom-inhibit-switch-buffer-hooks t)
         (keep t)
         (kill nil)
         buffer-list-update-hook
         )
    ;; (message "Trying to kill: %s" buf)
    (cond (in-other-windows ;; ignore if other windows have the buffer
           (message "Burying buffer as its in other windows")
           (bury-buffer)
           keep)
          ((and (doom-real-buffer-p buf)
                (buffer-modified-p buf) ;; check for modified buffers
                (not (y-or-n-p (format "Buffer %s is modified; kill anyway?" buf))))
           keep)
          ((not (cl-set-difference (doom-real-buffer-list)
                                   (doom-visible-buffers)))
           (message "No more buffers, jumping to fallback buffer")
           ;; No other buffers, go to fallback
           (switch-to-buffer (doom-fallback-buffer))
           kill)
          (t kill) ;; signal to kill the buffer as normal
          )
    )
  )

;;;###autoload
(defun +modeline-disable-icon-in-daemon-a (fn &rest args)
  ;; HACK Fix #4102 due to empty all-the-icons return value (caused by
  ;;      `doom--disable-all-the-icons-in-tty-a' advice) in tty daemon frames.
  (when (display-graphic-p)
    (apply fn args))
  )

;;;###autoload
(defun +modeline--inhibit-modification-hooks-a (fn &rest args)
  (with-silent-modifications (apply fn args))
  )
