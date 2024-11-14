;;; projects.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-projects-switch ()
  (run-hooks 'jg-projects-switch-hook)
  )

;;;###autoload
(defun +jg-projects-generic-command (_)
  ;; If fd exists, use it for git and generic projects. fd is a rust
  ;; program that is significantly faster than git ls-files or find, and
  ;; it respects .gitignore. This is recommended in the projectile docs.
  (cond
   ((when-let*
        ((bin (if (ignore-errors (file-remote-p default-directory nil t))
                  (cl-find-if (doom-rpartial #'executable-find t)
                              (list "fdfind" "fd"))
                doom-projectile-fd-binary))
         ;; REVIEW Temporary fix for #6618. Improve me later.
         (version (with-memoization doom-projects--fd-version
                    (cadr (split-string (cdr (doom-call-process bin "--version"))
                                        " " t))))
         ((ignore-errors (version-to-list version))))
      (concat (format "%s . -0 -H --color=never --type file --type symlink --follow --exclude .git %s"
                      bin (if (version< version "8.3.0")
                              "" "--strip-cwd-prefix"))
              (if IS-WINDOWS " --path-separator=/"))))
   ;; Otherwise, resort to ripgrep, which is also faster than find
   ((executable-find "rg" t)
    (concat "rg -0 --files --follow --color=never --hidden -g!.git"
            (if IS-WINDOWS " --path-separator=/")))
   ("find . -type f -print0")))

;;;###autoload
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

;;;###autoload
(defun +jg-projects-related-files-fn (path)
  " Given a relative path to a file, provide projectile with various :kinds of related file "
  (let ((impl-file  (f-join (f-parent (f-parent path)) (s-replace "test_" "" (f-filename path))))
        (test-file  (f-join (f-parent path) "__tests" (concat "test_" (f-filename path))))
        ;;(init-file  (f-join (f-parent path) "__init__.py"))
        (log-file   (f-join (projectile-project-root) (concat "log." (f-base path))))
        ;;(error-file (f-join (car (f-split path)) "errors" (concat (f-base path) "_errors.py")))
        (project    (f-join (projectile-project-root) "doot.toml"))
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

;;;###autoload
(defun +jg-projects-clean(arg)
  (interactive "P")
  (-when-let* ((root (projectile-project-root))
               (doot-toml (f-join root "doot.toml"))
               (dt-exists (f-exists? doot-toml))
               (default-directory root)
               )
    (let ((targets (+jg-projects-doot-tasks nil (lambda (x) (concat jg-projects-doot-cmd " clean " (when arg "-c ") (car (split-string x" " t " "))))))
          (counsel-compile--current-build-dir (or (counsel--compile-root) default-directory))
          )
      (ivy-read "Clean Task: " ivy-opts
                :action #'counsel-compile--action
                :caller 'jg-projects-clean)
      )
    )
  )

;;;###autoload
(defun +jg-projects-doot-tasks (&optional act-fn int-fn)
  " check for cache, if cache is newer than dodo file, use that, else run doit list "
  (let ((default-directory (or (projectile-project-root) default-directory))
        (act-fn (or act-fn (lambda (x) (concat jg-projects-doot-cmd " " (car (split-string x" " t " "))))))
        skip
        )
    (cond ((not (executable-find "doot"))
           (message "No Doot Command in Path")
           (setq skip t))
          ((not (f-exists? "doot.toml"))
           (message "No doot.toml Available")
           (setq skip t))
          ((and (f-exists? ".tasks_cache")
                (time-less-p (f-modification-time "doot.toml") (f-modification-time ".tasks_cache")))
           ;; No cache/out of date, so make it
           (message "Creating Cache")
           (+jg-projects-cache-tasks jg-projects-doot-cmd "list"))
          )

    (when (and (not skip) (f-exists? ".tasks_cache"))
      (with-temp-buffer
        (insert-file-contents ".tasks_cache")
        (+jg-projects-annotate-cmds (split-string (buffer-string) "\n" t " \n")
                                    act-fn int-fn
                                    )
        )
      )
    )
  )

;;;###autoload
(defun +jg-projects-cache-tasks (cmd &rest args)
  " run doit list, cache to .tasks_cache"
  (let ((proc (apply #'start-process "proc:doot:list" "*proc:doot:list*" cmd args))
        (sentinel (lambda (proc status)
                    (when (not (process-live-p proc))
                      (unless (f-exists? ".tasks_cache")
                        (f-touch ".tasks_cache"))

                      (with-current-buffer (process-buffer proc)
                        (write-region (point-min) (point-max) ".tasks_cache")
                        )
                      )
                    )
                  )
        )
    (set-process-sentinel proc sentinel)
    )
  )

;;;###autoload
(defun +jg-projects-detect-type ()
  (interactive)
  (setq projectile-project-type (projectile-detect-project-type))
  )

;;;###autoload
(defun +jg-projects-test-dir ()
  (interactive)
  (or
   (projectile-test-directory (+jg-projects-detect-type))
   "__tests"
   )
  )

(defvar jg-projects--test-files-cache nil)

;;;###autoload
(defun +jg-projects-invalidate-test-files-cache (&rest rst)
  (setq jg-projects--test-files-cache nil)
  )

;;;###autoload
(defun +jg-projects-test-files (&optional invalidate-cache)
  " A Caching version of projectile-find-test-file"
  (interactive "P")
  (projectile-maybe-invalidate-cache invalidate-cache)
  (when projectile-files-cache-expire
    (let ((cache-time
           (gethash (projectile-project-root) projectile-projects-cache-time)))
      (when (or (null cache-time)
                (< (+ cache-time projectile-files-cache-expire)
                   (projectile-time-seconds)))
        (projectile-invalidate-cache))))

  ;; Use the cache, if requested and available.
  (when projectile-enable-caching (setq files jg-projects--test-files-cache))

  (when (null files) (setq jg-projects--test-files-cache (projectile-current-project-test-files)))

  (let ((file (projectile-completing-read "Find test file: " files)))
    (find-file (expand-file-name file (projectile-project-root))))
)
