;; hooks.el<2> -*- lexical-binding: t; -*-
(require 'auto-revert)

;;;###autoload
(defun doom-auto-revert-buffer-h ()
  "Auto revert current buffer, if necessary."
  (unless (or auto-revert-mode (active-minibuffer-window))
    (let ((auto-revert-mode t))
      (auto-revert-handler)))
  )

;;;###autoload
(defun doom-auto-revert-buffers-h ()
  "Auto revert stale buffers in visible windows, if necessary."
  (dolist (buf (doom-visible-buffers))
    (with-current-buffer buf
      (doom-auto-revert-buffer-h)))
  )
