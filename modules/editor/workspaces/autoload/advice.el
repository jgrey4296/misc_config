;;; advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-projects-compile-adjust (cmd)
  "call cmd, but if it's got the text propery 'cmd use that instead "
  (let ((blddir (get-text-property 0 'blddir cmd))
        (bldenv (get-text-property 0 'bldenv cmd))
        (cmd-str cmd)
        (interactive nil)
        )
    (if (get-text-property 0 'recursive cmd)
        (counsel-compile blddir)
      (when (get-char-property 0 'cmd cmd)
        (setq cmd-str (get-text-property 0 'cmd cmd)))
      (setq interactive (get-char-property 0 'interactive cmd))

      (let ((default-directory (or blddir
                                   counsel-compile--current-build-dir
                                   default-directory))
            (compilation-environment bldenv))
        ;; No need to specify `:history' because of this hook.
        (add-hook 'compilation-start-hook #'counsel-compile--update-history)
        (unwind-protect
             (compile cmd-str interactive)
          (remove-hook 'compilation-start-hook #'counsel-compile--update-history))))))

;;;###autoload
(defun +jg-projects-command-expander (val)
  " Expand variables mentioned in the command "
  (let ((loc (if (eq major-mode 'dired-mode)
                 (dired-current-directory)
               (f-parent (buffer-file-name)))))
    (list (s-replace "\$" (format "TEST_TARGET=\"%s\"" loc) (car val)))
    )
  )

;;;###autoload
(defun +jg-projects-projectile-cmd-list  (fn &rest rst)
  " Use an ivy to get the command "
  (let* ((compilation-read-command nil)
        (root (projectile-project-root))
        (cmd-cache (f-join root jg-projects-cmd-cache-name))
        (candidates (if (and root (f-exists? cmd-cache))
                        (with-temp-buffer
                          (insert-file-contents cmd-cache)
                          (sort (s-split "\n" (buffer-string) t) 'string-lessp))))
        (ivy-val (ivy-read (plist-get rst :prompt-prefix)
                           candidates)))
    (if (not (-contains? candidates ivy-val))
        (append-to-file (format "%s\n" ivy-val) nil cmd-cache)
        )
    (apply fn (cons ivy-val (cdr rst)))
    )
  )

;;;###autoload
(advice-add 'counsel-compile--action :override #'+jg-projects-compile-adjust)

;;;###autoload
(advice-add 'projectile-run-compilation :filter-args #'+jg-projects-command-expander)

;;;###autoload
(advice-add 'projectile--run-project-cmd :around #'+jg-projects-projectile-cmd-list)


;;;###autoload
(defun +jg-workspaces-time-compile-cmd-retrieval (&optional dir)

  (let (cands times)
    (dolist (cmds counsel-compile-local-builds)
      (when (functionp cmds)
        (push (list (symbol-name cmds)
                    (benchmark-elapse
                      (setq cmds (funcall cmds dir)))) times))
      (when cmds
        (push (if (listp cmds) cmds (list cmds)) cands)))
    (cl-loop for pair in times
             do
             (message "%-10s : %s" (cadr pair) (car pair))
             )
    (apply #'append (nreverse cands)))
  )

;;;###autoload
(advice-add 'counsel--get-compile-candidates :override #'+jg-workspaces-time-compile-cmd-retrieval)
