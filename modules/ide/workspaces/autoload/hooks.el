;;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +workspaces-ensure-no-nil-workspaces-h (&rest _)
  ;;;; Create main workspace
  ;; The default perspective persp-mode creates is special and doesn't represent
  ;; a real persp object, so buffers can't really be assigned to it, among other
  ;; quirks, so I replace it with a "main" perspective.
  (when persp-mode
    (dolist (frame (frame-list))
      (when (string= (safe-persp-name (get-current-persp frame)) persp-nil-name)
        ;; Take extra steps to ensure no frame ends up in the nil perspective
        (persp-frame-switch (or (cadr (hash-table-keys *persp-hash*))
                                +workspaces-main)
                            frame)))))

;;;###autoload
(defun +workspaces-init-first-workspace-h (&rest _)
  "Ensure a main workspace exists."
  (when persp-mode
    (let (persp-before-switch-functions)
      ;; Try our best to hide the nil perspective.
      (when (equal (car persp-names-cache) persp-nil-name)
        (pop persp-names-cache))
      ;; ...and create a *real* main workspace to fill this role.
      (unless (or (persp-get-by-name +workspaces-main)
                  ;; Start from 2 b/c persp-mode counts the nil workspace
                  (> (hash-table-count *persp-hash*) 2))
        (persp-add-new +workspaces-main))
      ;; HACK Fix #319: the warnings buffer gets swallowed when creating
      ;;      `+workspaces-main', so display it ourselves, if it exists.
      (when-let (warnings (get-buffer "*Warnings*"))
        (save-excursion
          (display-buffer-in-side-window
           warnings '((window-height . shrink-window-if-larger-than-buffer))))))))

;;;###autoload
(defun +workspaces-init-persp-mode-h ()
  (cond (persp-mode
         ;; `uniquify' breaks persp-mode. It renames old buffers, which causes
         ;; errors when switching between perspective (their buffers are
         ;; serialized by name and persp-mode expects them to have the same
         ;; name when restored).
         (when uniquify-buffer-name-style
           (setq +workspace--old-uniquify-style uniquify-buffer-name-style))
         (setq uniquify-buffer-name-style nil)
         ;; Ensure `persp-kill-buffer-query-function' is last
         (remove-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function)
         (add-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function t)
         ;; Restrict buffer list to workspace
         (advice-add #'doom-buffer-list :override #'+workspace-buffer-list))
        (t
         (when +workspace--old-uniquify-style
           (setq uniquify-buffer-name-style +workspace--old-uniquify-style))
         (advice-remove #'doom-buffer-list #'+workspace-buffer-list))))

;;;###autoload
(defun +workspaces-save-winner-data-h (_)
  (when (and (bound-and-true-p winner-mode)
             (get-current-persp))
    (set-persp-parameter
     'winner-ring (list winner-currents
                        winner-ring-alist
                        winner-pending-undo-ring))))

;;;###autoload
(defun +workspaces-load-winner-data-h (_)
  (when (bound-and-true-p winner-mode)
    (cl-destructuring-bind
        (currents alist pending-undo-ring)
        (or (persp-parameter 'winner-ring) (list nil nil nil))
      (setq winner-undo-frame nil
            winner-currents currents
            winner-ring-alist alist
            winner-pending-undo-ring pending-undo-ring))))

;;;###autoload
(defun +workspaces-dead-buffer-p (buf)
  ;; Fix #1525: Ignore dead buffers in PERSP's buffer list
  (not (buffer-live-p buf)))

;;;###autoload
(defun +workspaces-remote-buffer-p (buf)
  ;; And don't save TRAMP buffers; they're super slow to restore
  (let ((dir (buffer-local-value 'default-directory buf)))
    (ignore-errors (file-remote-p dir))))

;;;###autoload
(defun +workspaces-reload-indirect-buffers-h (&rest _)
  (dolist (ibc +workspaces--indirect-buffers-to-restore)
    (cl-destructuring-bind (buffer-name . base-buffer-name) ibc
      (let ((base-buffer (get-buffer base-buffer-name)))
        (when (buffer-live-p base-buffer)
          (when (get-buffer buffer-name)
            (setq buffer-name (generate-new-buffer-name buffer-name)))
          (make-indirect-buffer base-buffer buffer-name t)))))
  (setq +workspaces--indirect-buffers-to-restore nil))

;;;###autoload
(defun +workspaces-delete-all-posframes-h (&rest _)
  (posframe-delete-all))

;;;###autoload
(defun +workspaces-add-current-buffer-h ()
  "Add current buffer to focused perspective."
  (or (not persp-mode)
      (persp-buffer-filtered-out-p
       (or (buffer-base-buffer (current-buffer))
           (current-buffer))
       persp-add-buffer-on-after-change-major-mode-filter-functions)
      (persp-add-buffer (current-buffer) (get-current-persp) nil nil)))

;;;###autoload
(defun +workspaces-set-up-tab-bar-integration-h ()
  (add-hook 'persp-before-deactivate-functions #'+workspaces-save-tab-bar-data-h)
  (add-hook 'persp-activated-functions #'+workspaces-load-tab-bar-data-h)
  ;; Load and save configurations for tab-bar.
  (add-hook 'persp-before-save-state-to-file-functions #'+workspaces-save-tab-bar-data-to-file-h)
  (+workspaces-load-tab-bar-data-from-file-h))

;;;###autoload
(defun doom-cleanup-project-cache-h ()
      "Purge projectile cache entries that:

a) have too many files (see `doom-projectile-cache-limit'),
b) represent blacklisted directories that are too big, change too often or are
   private. (see `doom-projectile-cache-blacklist'),
c) are not valid projectile projects."
      (when (and (bound-and-true-p projectile-projects-cache)
                 projectile-enable-caching)
        (setq projectile-known-projects
              (cl-remove-if #'projectile-ignored-project-p
                            projectile-known-projects))
        (projectile-cleanup-known-projects)
        (cl-loop with blacklist = (mapcar #'file-truename doom-projectile-cache-blacklist)
                 for proot in (hash-table-keys projectile-projects-cache)
                 if (or (not (stringp proot))
                        (string-empty-p proot)
                        (>= (length (gethash proot projectile-projects-cache))
                            doom-projectile-cache-limit)
                        (member (substring proot 0 -1) blacklist)
                        (and doom-projectile-cache-purge-non-projects
                             (not (doom-project-p proot)))
                        (projectile-ignored-project-p proot))
                 do (doom-log "Removed %S from projectile cache" proot)
                 and do (remhash proot projectile-projects-cache)
                 and do (remhash proot projectile-projects-cache-time)
                 and do (remhash proot projectile-project-type-cache))
        (projectile-serialize-cache)))
