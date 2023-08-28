;; advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +debugger--cleanup-after-realgud-a (&optional buf)
  "Kill command buffer when debugging session ends (which closes its popup)."
  (when (stringp buf)
    (setq buf (get-buffer buf)))
  (when-let (cmdbuf (realgud-get-cmdbuf buf))
    (let (kill-buffer-hook)
      (kill-buffer buf))))

;;;###autoload
(advice-add  'realgud:terminate :after #'+debugger--cleanup-after-realgud-a)

;; Monkey-patch `realgud:run-process' to run in a popup.
;; TODO Find a more elegant solution
;; FIXME Causes realgud:cmd-* to focus popup on every invocation

;;;###autoload
(defun +debugger--realgud-open-in-other-window-a
  (debugger-name script-filename cmd-args minibuffer-history-var &optional no-reset)
  (let* ((cmd-buf (apply #'realgud-exec-shell debugger-name script-filename
                         (car cmd-args) no-reset (cdr cmd-args)))
         (process (get-buffer-process cmd-buf)))
    (cond ((and process (eq 'run (process-status process)))
           (pop-to-buffer cmd-buf)
           (when (boundp 'evil-emacs-state-local-map)

             (define-key evil-emacs-state-local-map (kbd "ESC ESC") #'+debugger/quit))
           (realgud:track-set-debugger debugger-name)
           (realgud-cmdbuf-info-in-debugger?= 't)
           (realgud-cmdbuf-info-cmd-args= cmd-args)
           (when cmd-buf
             (switch-to-buffer cmd-buf)
             (when realgud-cmdbuf-info
               (let* ((info realgud-cmdbuf-info)
                      (cmd-args (realgud-cmdbuf-info-cmd-args info))
                      (cmd-str  (mapconcat #'identity cmd-args " ")))
                 (if (boundp 'starting-directory)
                     (realgud-cmdbuf-info-starting-directory= starting-directory))
                 (set minibuffer-history-var
                      (cl-remove-duplicates (cons cmd-str (eval minibuffer-history-var))
                                            :from-end t))))))
          (t
           (if cmd-buf (switch-to-buffer cmd-buf))
           (message "Error running command: %s" (mapconcat #'identity cmd-args " "))))
    cmd-buf))

;;;###autoload
(advice-add 'realgud:run-process :override #'+debugger--realgud-open-in-other-window-a)
