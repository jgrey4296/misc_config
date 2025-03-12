;;; +lsp.el -*- lexical-binding: t; -*-

(dlog! "Loading Python LSP")

;; (lsp-register-custom-settings
;;  '(("ruff...." t)
;;    )
(use-package! lsp-ruff :defer t)
(use-package! lsp-jedi :defer t)
(use-package! lsp-pylsp :defer t)

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

(speckler-setq! lsp-py-pyright ()
  ;; Pyright
  lsp-pyright-extra-paths #'[]
  lsp-pyright-venv-path conda-env-home-directory
  lsp-pyright-log-level "Information" ;; Error Warning Information Trace
  lsp-pyright-typechecking-mode "basic"
  )
(speckler-setq! lsp-py-ruff ()
  ;; Ruff
  lsp-ruff-advertize-fix-all            nil
  lsp-ruff-advertize-organize-imports   nil
  lsp-ruff-show-notifications           "always"
  lsp-ruff-log-level                    "info"
  lsp-ruff-server-command               '("ruff" "server")
  lsp-ruff-python-path                  "python3"
  lsp-ruff-import-strategy              "fromEnvironment"
  lsp-ruff-ruff-args                    nil
  )
(speckler-setq! lsp-py-pylsp ()
  :override t
  ;; pylsp
  lsp-pylsp-configuration-sources []
  lsp-pylsp-server-command '("pylsp")
  lsp-pylsp-rename-backend "jedi"
  lsp-pylsp-plist-value-when-compiled nil
  lsp-clients-pylsp-library-directories '("/usr/")
  lsp-pylsp-plugins-preload-modules nil

  ;; plugins enabled
  lsp-pylsp-plugins-ruff-enabled                         nil
  lsp-pylsp-plugins-mypy-enabled                         t
  lsp-pylsp-plugins-isort-enabled                        t

  lsp-pylsp-plugins-flake8-enabled                       nil
  lsp-pylsp-plugins-autopep8-enabled                     nil
  lsp-pylsp-plugins-yapf-enabled                         nil
  lsp-pylsp-plugins-pylint-enabled                       nil
  lsp-pylsp-plugins-pyflakes-enabled                     nil
  lsp-pylsp-plugins-pydocstyle-enabled                   nil
  lsp-pylsp-plugins-pycodestyle-enabled                  nil
  lsp-pylsp-plugins-preload-enabled                      nil
  lsp-pylsp-plugins-mccabe-enabled                       nil
  lsp-pylsp-plugins-black-enabled                        nil

  ;; flake8
  lsp-pylsp-plugins-flake8-config          nil
  lsp-pylsp-plugins-flake8-exclude         nil
  lsp-pylsp-plugins-flake8-filename        nil
  lsp-pylsp-plugins-flake8-hang-closing    nil
  lsp-pylsp-plugins-flake8-ignore          nil
  lsp-pylsp-plugins-flake8-max-line-length nil
  lsp-pylsp-plugins-flake8-select          nil

  ;; jedi
  lsp-pylsp-plugins-jedi-completion-fuzzy                  nil
  lsp-pylsp-plugins-jedi-definition-follow-builtin-imports nil
  lsp-pylsp-plugins-jedi-definition-follow-imports         t
  lsp-pylsp-plugins-jedi-environment                       nil
  lsp-pylsp-plugins-jedi-use-pyenv-environment             nil
  lsp-pylsp-plugins-jedi-symbols-enabled                  t
  lsp-pylsp-plugins-jedi-hover-enabled                    t
  lsp-pylsp-plugins-jedi-references-enabled               t
  lsp-pylsp-plugins-jedi-signature-help-enabled           t
  lsp-pylsp-plugins-jedi-definition-enabled               t
  lsp-pylsp-plugins-jedi-completion-enabled               t
  lsp-pylsp-plugins-jedi-completion-include-class-objects t
  lsp-pylsp-plugins-jedi-completion-include-params        t
  lsp-pylsp-plugins-jedi-symbols-all-scopes                nil

  ;; mccabe
  lsp-pylsp-plugins-mccabe-threshold 15

  ;; mypy
  lsp-pylsp-plugins-mypy-config-sub-paths  nil
  lsp-pylsp-plugins-mypy-dmypy             nil
  lsp-pylsp-plugins-mypy-dmypy-status-file nil
  lsp-pylsp-plugins-mypy-exclude           nil
  lsp-pylsp-plugins-mypy-live-mode         t
  lsp-pylsp-plugins-mypy-overrides         nil
  lsp-pylsp-plugins-mypy-report-progress   t
  lsp-pylsp-plugins-mypy-strict            nil

  ;; pycodestyle
  lsp-pylsp-plugins-pycodestyle-exclude         nil
  lsp-pylsp-plugins-pycodestyle-filename        nil
  lsp-pylsp-plugins-pycodestyle-hang-closing    nil
  lsp-pylsp-plugins-pycodestyle-ignore          nil
  lsp-pylsp-plugins-pycodestyle-max-line-length nil
  lsp-pylsp-plugins-pycodestyle-select          nil
  lsp-pylsp-plugins-pydocstyle-add-ignore       nil
  lsp-pylsp-plugins-pydocstyle-add-select       nil
  lsp-pylsp-plugins-pydocstyle-convention       nil
  lsp-pylsp-plugins-pydocstyle-ignore           nil
  lsp-pylsp-plugins-pydocstyle-match            nil
  lsp-pylsp-plugins-pydocstyle-match-dir        nil
  lsp-pylsp-plugins-pydocstyle-select           nil

  ;; pylint
  lsp-pylsp-plugins-pylint-args nil

  ;; rope
  lsp-pylsp-plugins-rope-autoimport-memory nil
  lsp-pylsp-plugins-rope-completion-eager nil
  lsp-pylsp-plugins-rope-completion-enabled              nil
  lsp-pylsp-plugins-rope-autoimport-code-actions-enabled t
  lsp-pylsp-plugins-rope-autoimport-completions-enabled  t
  lsp-pylsp-plugins-rope-autoimport-enabled              t
  lsp-pylsp-rope-extension-modules nil
  lsp-pylsp-rope-rope-folder       ".temp/rope"

  ;; ruff
  lsp-pylsp-plugins-ruff-config           "ruff.toml"
  lsp-pylsp-plugins-ruff-exclude          nil
  lsp-pylsp-plugins-ruff-executable       "ruff"
  lsp-pylsp-plugins-ruff-extend-ignore    nil
  lsp-pylsp-plugins-ruff-extend-select    nil
  lsp-pylsp-plugins-ruff-format           nil
  lsp-pylsp-plugins-ruff-ignore           nil
  lsp-pylsp-plugins-ruff-line-length      nil
  lsp-pylsp-plugins-ruff-per-file-ignores nil
  lsp-pylsp-plugins-ruff-preview          nil
  lsp-pylsp-plugins-ruff-select           nil
  lsp-pylsp-plugins-ruff-severities       nil
  lsp-pylsp-plugins-ruff-target-version   nil
  lsp-pylsp-plugins-ruff-unsafe-fixes     nil

  )

(speckler-setq! lsp-py-jedi ()
  lsp-jedi-enable                            t
  lsp-jedi-startup-message                   t
  lsp-jedi-markup-kind-preferred             nil
  lsp-jedi-trace-server                      "off" ;; off | message  | verbose
  lsp-jedi-auto-import-modules               []
  lsp-jedi-case-insensitive-completion       t
  lsp-jedi-debug                             nil
  lsp-jedi-executable-command                nil
  lsp-jedi-executable-args                   nil
  lsp-jedi-code-action-name-extract-function nil
  lsp-jedi-code-action-name-extract-variable nil
  lsp-jedi-completion-disable-snippets nil
  lsp-jedi-completion-resolve-eagerly  nil
  lsp-jedi-diagnostics-enable          t
  lsp-jedi-diagnostics-did-open        t
  lsp-jedi-diagnostics-did-change      nil
  lsp-jedi-diagnostics-did-save        t
  lsp-jedi-hover-enable                      t
  lsp-jedi-hover-disable-keyword-all         nil
  lsp-jedi-hover-disable-keyword-names       []
  lsp-jedi-hover-disable-keyword-full-names  []
  lsp-jedi-workspace-extra-paths             []
  lsp-jedi-workspace-symbols-max-symbols     nil
  lsp-jedi-workspace-symbols-ignore-folders  [".nox" ".tox" ".venv" "__pycache__" "venv"]
  )
