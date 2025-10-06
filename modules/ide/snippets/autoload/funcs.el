;;; funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-snippets-complete-or-snippet (&optional arg)
  (interactive "p")
  (cond ((company-complete-common-or-cycle) nil)
        ((yas-expand-from-trigger-key) nil)
        (t (indent-for-tab-command))
      )
    )

;;;###autoload
(defun +jg-snippets-new-snippet()
  "Create a new snippet in `+snippets-dir'."
  (interactive)
  (let ((default-directory (expand-file-name (symbol-name major-mode) +snippets-dir)))
    (+jg-snippets--ensure-dir default-directory)
    (with-current-buffer (pop-to-buffer "untitled-snippet")
      (snippet-mode)
      (erase-buffer)
      (+file-templates--expand t :mode 'snippet-mode)
      (when (bound-and-true-p evil-local-mode)
        (evil-insert-state))))
  )

;;;###autoload
(defun +jg-snippets--ensure-dir (dir)
  (unless (file-directory-p dir)
    (if (y-or-n-p (format "%S doesn't exist. Create it?" (abbreviate-file-name dir)))
        (make-directory dir t)
      (error "%S doesn't exist" (abbreviate-file-name dir)))))

;;;###autoload
(defun +jg-snippets-debug-dirs ()
  (interactive)
  (message (string-join yas-snippet-dirs "\n"))
  )
