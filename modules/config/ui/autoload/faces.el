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
