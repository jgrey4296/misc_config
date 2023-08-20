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
