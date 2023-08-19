;;; +advice.el -*- lexical-binding: t; -*-

;;;###autoload
(advice-add '+eval--ensure-in-repl-buffer :filter-return #'+jg-repl-fix)

;;;###autoload
(defun +jg-repl-fix (result)
  (message "Repl Result: %s" result)
  (if (bufferp result)
      (with-current-buffer result
        (let* ((mode (intern-soft (s-replace "inferior-" "" (symbol-name major-mode))))
               (mode2 (intern-soft (format "inferior-%s" mode)))
               (project-root (doom-project-root))
               )
          (if (and mode (not (gethash (cons mode project-root) +eval-repl-buffers)))
              (puthash (cons mode project-root) result +eval-repl-buffers))
          (if (and mode2 (not (gethash (cons mode2 project-root) +eval-repl-buffers)))
              (puthash (cons mode2 project-root) result +eval-repl-buffers))
          )
        )
    )
  result
  )

;;;###autoload
(advice-add '+jg-send-region-to-repl :filter-args #'+jg-advice-send-repl-auto-line)

;;;###autoload
(defun +jg-advice-send-repl-auto-line (args)
  " Handle visual-mode variance for send-region-to-repl "
  (if (not (eq evil-state 'visual))
      (list (line-beginning-position) (line-end-position) (if (eq (length args) 3)  (last args) nil))
    args)
  )

;;;###autoload
(advice-add 'lsp-diagnostics-flycheck-enable :around #'+lsp--respect-user-defined-checkers-a)

;;;###autoload
(defun +lsp--respect-user-defined-checkers-a (fn &rest args)
  "Ensure user-defined `flycheck-checker' isn't overwritten by `lsp'."
  (if flycheck-checker
      (let ((old-checker flycheck-checker))
        (apply fn args)
        (setq-local flycheck-checker old-checker))
    (apply fn args)))

;;;###autoload
(advice-add 'lsp-describe-session :around #'+jg-lsp-dont-select-session)

;;;###autoload
(defun +jg-lsp-dont-select-session (fn &rest args)
  " Dont auto select lsp session buffer "
  (let ((curr (selected-window)))
    (apply fn args)
    (select-window curr)
    )
  )

;;;###autoload
(advice-add 'lsp--shutdown-workspace :around #'+lsp-defer-server-shutdown-a)

;;;###autoload
(defun +lsp-defer-server-shutdown-a (fn &optional restart)
  "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
  (when (timerp +lsp--deferred-shutdown-timer)
    (cancel-timer +lsp--deferred-shutdown-timer)
    (setq +lsp--deferred-shutdown-timer nil))
  (cond (restart
         (prog1 (funcall fn restart)
           (+lsp-optimization-mode -1)))

        ((and lsp-keep-workspace-alive
              (numberp +lsp-defer-shutdown)
              (> +lsp-defer-shutdown 0))
         (message "Deferring shutdown")
         (setq +lsp--deferred-shutdown-timer
               (run-with-timer (or +lsp-defer-shutdown 3) nil
                            #'+lsp-defer-shutdown-action
                            lsp--cur-workspace fn)))

        (t (prog1 (funcall fn restart)
             (+lsp-optimization-mode -1)))
        )
  )

(defun +lsp-defer-shutdown-action (workspace fn)
  (message "Shutting down Delayed LSP Workspace :%s" (lsp--workspace-print workspace))
  (with-lsp-workspace workspace
    (message "checking for buffers")
    (let ((buffers (lsp--workspace-buffers workspace))
          (lsp-restart 'ignore)
          )
      (message "Workspace Buffers: %s" buffers)
      (unless buffers
        (message "Calling shutdown")
        (funcall fn)
        (+lsp-optimization-mode -1))))
  )

;;;###autoload
(advice-add 'lsp--auto-configure :around #'+lsp--use-hook-instead-a)

;;;###autoload
(defun +lsp--use-hook-instead-a (fn &rest args)
    "Change `lsp--auto-configure' to not force `lsp-ui-mode' on us. Using a hook
instead is more sensible."
    (letf! ((#'lsp-ui-mode #'ignore))
      (apply fn args)))

;;;###autoload
(advice-add 'eglot--managed-mode :around #'+lsp--defer-server-shutdown-a)

;;;###autoload
(defun +lsp--defer-server-shutdown-a (fn &optional server)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
    (letf! (defun eglot-shutdown (server)
             (if (or (null +lsp-defer-shutdown)
                     (eq +lsp-defer-shutdown 0))
                 (prog1 (funcall eglot-shutdown server)
                   (+lsp-optimization-mode -1))
               (run-at-time
                (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
                nil (lambda (server)
                      (unless (eglot--managed-buffers server)
                        (prog1 (funcall eglot-shutdown server)
                          (+lsp-optimization-mode -1))))
                server)))
      (funcall fn server)))

;;;###autoload
(defun +lsp--log-diagnostic-build (&rest args)
  (message "Building Diagnostics: %s" args)
  )

;;;###autoload
(advice-add 'lsp-diagnostics--flycheck-level :before #'+lsp--log-diagnostic-build)
