;;; +lsp.el -*- lexical-binding: t; -*-

(dlog! "Loading Python LSP")

(use-package! lsp-ruff
  :defer t
  :config

  ;; (lsp-register-custom-settings
  ;;  '(("ruff...." t)
  ;;    )
   )

(use-package! lsp-pylsp
  :defer t
  )

(speckler-add! lib-env ()
  :override nil
  `(py-lsp
    :lang python
    :setup #'(lambda (state &rest rest) (require 'lsp-mode) (require 'lsp-ruff) (require 'lsp-pylsp))
    :start #'(lambda (state &rest rest)
               (add-hook 'python-mode-hook #'lsp-deferred)
               (add-hook 'python-ts-mode-hook #'lsp-deferred)
               )
    :stop  #'(lambda (state &rest rest)
               (remove-hook 'python-mode-hook #'lsp-deferred)
               (remove-hook 'python-ts-mode-hook #'lsp-deferred)
               )
    )
  )

(speckler-setq! lsp-py ()
  ;; Pyright
  lsp-pyright-extra-paths #'[]
  lsp-pyright-venv-path conda-env-home-directory
  lsp-pyright-log-level "Information" ;; Error Warning Information Trace
  lsp-pyright-typechecking-mode "basic"
  ;; Ruff
  lsp-ruff-advertize-fix-all            nil
  lsp-ruff-advertize-organize-imports   nil
  lsp-ruff-show-notifications           "always"
  lsp-ruff-log-level                    "info"
  lsp-ruff-server-command               '("ruff" "server")
  lsp-ruff-python-path                  "python3"
  lsp-ruff-import-strategy              "fromEnvironment"
  lsp-ruff-ruff-args                    nil
  ;; pylsp
  lsp-pylsp-configuration-sources []
  lsp-pylsp-server-command '("pylsp")
  lsp-pylsp-plugins-flake8-enabled nil
  )
