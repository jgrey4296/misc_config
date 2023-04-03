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
  (cond ((and (modulep! :completion vertico)
              (modulep! :tools lsp)
              (bound-and-true-p lsp-mode))
         (consult-lsp-diagnostics arg))
        ((and (modulep! :checkers syntax)
              (bound-and-true-p flycheck-mode))
         (if (modulep! :completion vertico)
             (consult-flycheck)
           (flycheck-list-errors)))
        ((bound-and-true-p flymake-mode)
         (if (modulep! :completion vertico)
             (consult-flymake)
           (flymake-show-diagnostics-buffer)))
        (t
         (user-error "No diagnostics backend detected. Enable flycheck or \
flymake, or set up lsp-mode if applicable (see :lang lsp)"))))
