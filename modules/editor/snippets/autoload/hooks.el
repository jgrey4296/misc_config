;;; hooks.el -*- lexical-binding: t; -*-

;;; Hooks

;;;###autoload
(defun +snippets-enable-project-modes-h (mode &rest _)
  "Automatically enable snippet libraries for project minor modes defined with
`def-project-mode!'."
  (if (symbol-value mode)
      (yas-activate-extra-mode mode)
    (yas-deactivate-extra-mode mode)))

;;;###autoload
(defun +snippets-read-only-maybe-h ()
  "Enable `read-only-mode' if snippet is built-in."
  (when (file-in-directory-p default-directory doom-local-dir)
    (read-only-mode 1)
    (message "This is a built-in snippet, enabling read only mode. Use `yas-new-snippet' to redefine snippets")))
