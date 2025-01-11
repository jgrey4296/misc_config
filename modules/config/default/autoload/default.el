;; config/default/autoload/default.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +default/restart-server ()
  "Restart the Emacs server."
  (interactive)
  (server-force-delete)
  (while (server-running-p)
    (sleep-for 1))
  (server-start))
