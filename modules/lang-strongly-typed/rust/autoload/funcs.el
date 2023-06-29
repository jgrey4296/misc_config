;;; lang/rust/autoload.el -*- lexical-binding: t; -*-

;; TODO (defun +rust/run-cargo () (interactive))

;;;###autoload
(defun +rust-cargo-project-p ()
  "Return t if this is a cargo project."
  (locate-dominating-file buffer-file-name "Cargo.toml"))


;;
;;; Custom Cargo commands

(autoload 'rustic-run-cargo-command "rustic-cargo")

;;;###autoload
(defun +rust/cargo-audit ()
  "Run 'cargo audit' for the current project."
  (interactive)
  (rustic-run-cargo-command "cargo audit")
  )

;;;###autoload
(defun +rust--dont-install-packages-a (&rest _)
  ;; HACK If lsp/eglot isn't available, it attempts to install lsp-mode via
  ;;   package.el. Doom manages its own dependencies through straight so disable
  ;;   this behavior to avoid package-not-initialized errors.
  (message "No LSP server running")
  )

;;;###autoload
(advice-add 'rustic-install-lsp-client-p :override #'+rust--dont-install-packages-a)

;;;###autoload
(defun +jg-rust-related-files-fn (path)
    " Given a relative path to a file, provide projectile with various :kinds of related file "
    (let ((impl-file  (f-join (f-parent (f-parent path)) (s-replace "test_" "" (f-filename path))))
          (test-file  (f-join (f-parent path) "__tests" (concat "test_" (f-filename path))))
          ;;(init-file  (f-join (f-parent path) "__init__.py"))
          (log-file   (f-join (projectile-project-root) (concat "log." (f-base path))))
          ;;(error-file (f-join (car (f-split path)) "errors" (concat (f-base path) "_errors.py")))
          (project    (f-join (projectile-project-root) "Cargo.toml"))
          (is-test (s-matches? "^test_" (f-filename path)))
          )
      (append (when is-test (list :impl impl-file))
              (unless is-test (list :test test-file))
              (when (s-matches? "\/cli\/" path) (list :project project))
              (list :init-py init-file)
              (list :log log-file)
              (list :errors error-file)
              )
      )
    )
