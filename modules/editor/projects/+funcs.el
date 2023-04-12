;;; +funcs.el -*- lexical-binding: t; -*-

(define-advice counsel-compile--action (:override (cmd)
                                                  +jg-projects-compile-adjust)
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

(define-advice projectile-run-compilation (:filter-args (val)
                                           +jg-projects-command-expander)
  " Expand variables mentioned in the command "
  (let ((loc (if (eq major-mode 'dired-mode)
                 (dired-current-directory)
               (f-parent (buffer-file-name)))))
    (list (s-replace "\$" (format "TEST_TARGET=\"%s\"" loc) (car val)))
    )
  )

(define-advice projectile--run-project-cmd (:around (fn &rest rst)
                                            +jg-projects-projectile-cmd-list)
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

(defun +jg-projects-open-configs ()
  (interactive)
  (let ((default-directory (projectile-project-root))
        (current-window (selected-window))
        configs
        )
    (setq configs (-filter #'f-exists? projectile-project-root-files))
    (message "Existing: " configs)
    (cl-loop for file in configs
             do
             (with-selected-window current-window
               (find-file file)
               (setq current-window (split-window-right))
               )
             )
    (when configs
      (delete-window current-window)
      )
    (balance-windows)
    )
  )

(defun +jg-projects-related-files-fn (path)
  " Given a relative path to a file, provide projectile with various :kinds of related file "
  (let ((impl-file  (f-join (f-parent (f-parent path)) (s-replace "test_" "" (f-filename path))))
        (test-file  (f-join (f-parent path) "__tests" (concat "test_" (f-filename path))))
        ;;(init-file  (f-join (f-parent path) "__init__.py"))
        (log-file   (f-join (projectile-project-root) (concat "log." (f-base path))))
        ;;(error-file (f-join (car (f-split path)) "errors" (concat (f-base path) "_errors.py")))
        (project    (f-join (projectile-project-root) "dooter.py"))
        (is-test (s-matches? "^test_" (f-filename path)))
        )
    (append (when is-test (list :impl impl-file))
            (unless is-test (list :test test-file))
            (when (s-matches? "\/cli\/" path) (list :project project))
            (list :init-py init-file)
            (list :log log-file)
            (list :errors error-file)
            )
    )
  )

(defun +jg-projects-clean(arg)
  (interactive "P")
  (let ((default-directory (projectile-project-root))
        ivy-opts
        )
    (setq ivy-opts (+jg-projects-doot-tasks nil (lambda (x) (concat jg-projects-doot-cmd " clean " (when arg "-c ") (car (split-string x" " t " ")))))
          counsel-compile--current-build-dir (or (counsel--compile-root) default-directory))
    (ivy-read "Clean Task: " ivy-opts
              :action #'counsel-compile--action
              :caller 'jg-projects-clean)
    )
  )

(defun +jg-projects-doot-tasks (&optional dir act-fn int-fn)
  ;; check for cache, if cache is newer than dodo file, use that, else run doit list
  (let ((default-directory (or dir (projectile-project-root) default-directory))
        (act-fn (or act-fn (lambda (x) (concat jg-projects-doot-cmd " " (car (split-string x" " t " "))))))
        )
    (unless (and (f-exists? ".tasks_cache")
                 (time-less-p (f-modification-time "dooter.py") (f-modification-time ".tasks_cache")))
        ;; No cache/out of date, so make it
      (message "Creating Cache")
      (+jg-projects-cache-tasks jg-projects-doot-cmd "list")
      )

    (with-temp-buffer
      (insert-file-contents ".tasks_cache")
      (+jg-projects-annotate-cmds (split-string (buffer-string) "\n" t " \n")
                                 act-fn int-fn
                                 )
      )
    )
  )

(defun +jg-projects-annotate-cmds (cmds act-fn &optional int-fn)
  (cl-loop for cmd in cmds
           collect (let ((cmd-str cmd)
                         (cmd-act (funcall act-fn cmd))
                         (interactive (when int-fn (funcall int-fn cmd)))
                         )
                     ;; The cmd text prop is what is actually run, the text itself is what is shown
                     (set-text-properties 0 (length cmd-str) `(cmd ,cmd-act
                                                               interactive ,interactive
                                                               ) cmd-str)
                     cmd-str))
  )

(defun +jg-projects-cache-tasks (cmd &rest args)
  " run doit list, cache to .tasks_cache"
  (let (result-code result-text results)
    (with-temp-buffer
      (setq result-code (apply 'call-process cmd nil (current-buffer) nil args)
            result-text (buffer-string)
            )
      (write-region (point-min) (point-max) ".tasks_cache")
      )
    )
  )

(defun +jg-projects-detect-type ()
  (interactive)
  (message "Project Type: %s" (projectile-detect-project-type default-directory))

  )
