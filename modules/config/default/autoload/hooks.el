;; hooks.el -*- lexical-binding: t; -*-

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

;;;###autoload
(defun doom--recentf-touch-buffer-h ()
      "Bump file in recent file list when it is switched or written to."
      (when buffer-file-name
        (recentf-add-file buffer-file-name))
      ;; Return nil for `write-file-functions'
      nil)

;;;###autoload
(defun doom--recentf-add-dired-directory-h ()
      "Add dired directories to recentf file list."
      (recentf-add-file default-directory))

;;;###autoload
(defun doom-savehist-unpropertize-variables-h ()
  "Remove text properties from `kill-ring' to reduce savehist cache size."
  (setq kill-ring
        (mapcar #'substring-no-properties
                (cl-remove-if-not #'stringp kill-ring))
        register-alist
        (cl-loop for (reg . item) in register-alist
                 if (stringp item)
                 collect (cons reg (substring-no-properties item))
                 else collect (cons reg item)))
  )

;;;###autoload
(defun doom-savehist-remove-unprintable-registers-h ()
  "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
  ;; Save new value in the temp buffer savehist is running
  ;; `savehist-save-hook' in. We don't want to actually remove the
  ;; unserializable registers in the current session!
  (setq-local register-alist
              (cl-remove-if-not #'savehist-printable register-alist))
  )
