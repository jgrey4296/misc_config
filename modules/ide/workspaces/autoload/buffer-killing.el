;;; buffer-killing.el -*- lexical-binding: t; -*-



;; kill-buffer-hook s

(defun +jg-workspaces-remove-dired-from-persps ()
  (-when-let* ((is-dired (eq major-mode 'dired-mode))
               (buf (current-buffer))
               )
    (cl-loop for persp in (persp-other-persps-with-buffer-except-nil buf)
             do
             (kill-buffer buf)
             )
    )
  )

(defun +jg-workspaces-fallback ()
  (-when-let* ((root (not (projectile-project-root)))
               (other-buffers (not (-filter #'doom-real-buffer-p (buffer-list))))
               )
    (switch-to-buffer (doom-fallback-buffer))
    )
  )

(defun +jg-workspaces-fallback-to-project-root ()
  (-when-let* ((root (projectile-project-root))
               (other-buffers (not (-filter #'doom-real-buffer-p (project-buffers (project-current)))))
               )
    (find-file root)
    )
  )

;; kill-buffer-query-functions
;; fns with no args, buffer is current
;; return true to kill, nil to keep

(defvar jg-worksapces-buffer-protect-list nil)

(defun +jg-worksapces-protect-buffers ()
  t
  )

(defun +jg-workspaces-protect-multi-window-buffers ()
  (let ((other-windows (delq (selected-window)
                             (get-buffer-window-list (current-buffer) nil t)))
        )
    (if (not other-windows)
        t
      (bury-buffer)
      nil
      )
    )
  )

(defun +jg-workspaces-protect-modified-buffers ()
  (if (and (doom-real-buffer-p (current-buffer))
           (buffer-modified-p (current-buffer)))
      (y-or-n-p (format "JG: Buffer %s is modified; kill anyway?" (current-buffer)))
    t
    )
  )

(defun +jg-workspaces-protect-project-files ()
  t
  )

(defun +jg-workspaces-protect-processes ()
  "Ask before killing a buffer that has a running process."
  (let ((process (get-buffer-process (current-buffer))))
    (or (not process)
        (not (memq (process-status process) '(run stop open listen)))
        (not (process-query-on-exit-flag process))
        (yes-or-no-p
	 (format "Buffer %S has a running process; kill it? "
		 (buffer-name (current-buffer)))))))


;; process killing
