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
