;;; advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom--projectile-dirconfig-file-a ()
  ;; Support the more generic .project files as an alternative to .projectile
  (cond ((file-exists-p! (or ".projectile" ".project") (projectile-project-root)))
        ((expand-file-name ".project" (projectile-project-root))))
  )

;;;###autoload
(advice-add 'projectile-dirconfig-file :override #'doom--projectile-dirconfig-file-a)

;; HACK Don't rely on VCS-specific commands to generate our file lists. That's
;;      7 commands to maintain, versus the more generic, reliable and
;;      performant `fd' or `ripgrep'.

;;;###autoload
(defun doom--only-use-generic-command-a (fn vcs)
  "Only use `projectile-generic-command' for indexing project files.
And if it's a function, evaluate it."
  (if (and (functionp projectile-generic-command)
           (not (file-remote-p default-directory)))
      (funcall projectile-generic-command vcs)
    (let ((projectile-git-submodule-command
           (get 'projectile-git-submodule-command 'initial-value)))
      (funcall fn vcs))))

;;;###autoload
(advice-add 'projectile-get-ext-command :around #'doom--only-use-generic-command-a)

;;;###autoload
(defun doom--projectile-default-generic-command-a (fn &rest args)
  "If projectile can't tell what kind of project you're in, it issues an error
when using many of projectile's command, e.g. `projectile-compile-command',
`projectile-run-project', `projectile-test-project', and
`projectile-configure-project', for instance.

This suppresses the error so these commands will still run, but prompt you for
the command instead."
  (ignore-errors (apply fn args)))

;;;###autoload
(advice-add 'projectile-default-generic-command :around #'doom--projectile-default-generic-command-a)

;;;###autoload
(defun +workspaces--evil-alternate-buffer-a (&optional window)
  "Make `evil-alternate-buffer' ignore buffers outside the current workspace."
  (let* ((prev-buffers
          (if persp-mode
              (cl-remove-if-not #'persp-contain-buffer-p (window-prev-buffers)
                                :key #'car)
            (window-prev-buffers)))
         (head (car prev-buffers)))
    (if (eq (car head) (window-buffer window))
        (cadr prev-buffers)
      head)))

;;;###autoload
(defun +workspaces-remove-dead-buffers-a (persp)
  " HACK Fixes #4196, #1525: selecting deleted buffer error when quitting Emacs
        or on some buffer listing ops. "
  (when (perspective-p persp)
    ;; HACK Can't use `persp-buffers' because of a race condition with its gv
    ;;      getter/setter not being defined in time.
    (setf (aref persp 2)
          (cl-delete-if-not #'persp-get-buffer-or-null (persp-buffers persp)))))

;;;###autoload
(advice-add 'evil-alternate-buffer :override #'+workspaces--evil-alternate-buffer-a)

;;;###autoload
(advice-add 'persp-buffers-to-savelist :before #'+workspaces-remove-dead-buffers-a)
