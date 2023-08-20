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

;;;###autoload
(defun +snippets--disable-smartparens-before-expand-h ()
        ;; Remember the initial smartparens state only once, when expanding a
        ;; top-level snippet.
        (unless +snippets--expanding-p
          (setq +snippets--expanding-p t
                +snippets--smartparens-enabled-p smartparens-mode))
        (when smartparens-mode
          (smartparens-mode -1))
        )

;;;###autoload
(defun +snippets--restore-smartparens-after-expand-h ()
  ;; Is called only for the top level snippet, but not for the nested ones.
  ;; Hence `+snippets--expanding-p'.
  (setq +snippets--expanding-p nil)
  (when +snippets--smartparens-enabled-p
    (smartparens-mode 1))
  )
