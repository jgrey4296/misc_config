;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-ui-what-face (pos)
  ;; from: http://stackoverflow.com/questions/1242352/
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;;###autoload
(defun +jg-ui-face-under-cursor-customize (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (customize-face face) (message "No face at %d" pos))))

;;;###autoload
(defun +jg-ui-toggle-docstrings ()
  (interactive)
  (setq which-key-show-docstrings
        (if which-key-show-docstrings
            nil
          'docstring-only
            )
        )
  )

;;;###autoload
(defun +jg-ui-insert-faces ()
  "insert lisp code for a set of faces automatically"
  (interactive)
  (let ((name (read-string "Face Names: "))
        (num (read-number "Number of Faces: "))
        (file (read-file-name "File: " jg-ui-default-face-gen-palette-dir))
        colors
        )

    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "#[[:alnum:]]+" nil t)
        (push (match-string 0) colors)
        ))
    (cl-assert (<= num (length colors)))

    (cl-loop for n to (- num 1) do
          (insert "(defface " name "-face-" (number-to-string n) "\n")
          (insert "  '((t :foreground \"" (nth n colors) "\"))\n")
          (insert "  \"Generated " name " " (number-to-string n) " Face\"\n)\n\n")
          )
    )
  )

(defvar jg-jit-lock-debug-time 1)

;;;###autoload
(define-minor-mode jg-jit-lock-debug-mode
  "Minor mode to help debug code run from jit-lock.

When this minor mode is enabled, jit-lock runs as little code as possible
during redisplay and moves the rest to a timer, where things
like `debug-on-error' and Edebug can be used."
  :global t
  (when jit-lock-defer-timer
    (cancel-timer jit-lock-defer-timer)
    (setq jit-lock-defer-timer nil))
  (when jg-jit-lock-debug-mode
    (setq jit-lock-defer-timer
          (run-with-idle-timer jg-jit-lock-debug-time t #'jit-lock--debug-fontify))))

(defun jg-jit-lock-debug-announce (&rest args)
  (message "Jit Locking: %s" jit-lock-defer-buffers)
  )

(advice-add #'jit-lock--debug-fontify :before #'jg-jit-lock-debug-announce)
