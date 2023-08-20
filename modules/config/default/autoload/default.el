;; config/default/autoload/default.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +default/restart-server ()
  "Restart the Emacs server."
  (interactive)
  (server-force-delete)
  (while (server-running-p)
    (sleep-for 1))
  (server-start))

;;;###autoload
(defun +default/diagnostics (&rest arg)
  "List diagnostics for the current buffer/project.
If the the vertico and lsp modules are active, list lsp diagnostics for the
current project. Otherwise list them for the current buffer"
  (interactive)
  (flycheck-list-errors)
)
