;;; overrides.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-ide-support-clear-lsp-cache ()
  (interactive)
  (when (f-exists? lsp-session-file)
    (f-delete lsp-session-file))
  (setq lsp--session (make-lsp-session))

  )

;;;###autoload
(defun +jg-ide-end-workspace-process ()
  (interactive)
  (message "There are %s workspaces here" (length (lsp-workspaces)))
  (-each (lsp-workspaces)
    #'(lambda (wsp)
        (let* ((proc (lsp--workspace-proc wsp))
               (is-proc (processp proc))
               (is-alive (process-live-p proc))
               )
          (message "Killing Workspace proc: %s : %s : %s" proc is-proc is-alive)
          (when is-alive
            (kill-process proc))
          )

        (let* ((proc (lsp--workspace-cmd-proc wsp))
               (is-proc (processp proc))
               (is-alive (process-live-p proc))
               )
          (message "Killing Workspace cmd proc: %s : %s : %s" proc is-proc is-alive)
          (when is-alive
            (kill-process proc))
          )
        (with-lsp-workspace wsp
          (lsp--shutdown-workspace))
        )
    )
  )

;;;###autoload
(defun +jg-ide-debug-lsp (&optional arg)
  (interactive "p")
  (pcase arg
    (4
     (message "Available Clients:\n%s"
              (hash-table-keys lsp-clients))
     )
    ((or 'nil 1)
     (message "Number of LSP Workspaces: %s\nWorkspace Servers:%s\nAvailable Clients: %s"
              (length (lsp-workspaces))
              (mapcar
               #'(lambda (x)
                   (lsp--workspace-server-id x))
               (lsp-workspaces))
              (hash-table-count lsp-clients)
              )
     )
    )
  )
