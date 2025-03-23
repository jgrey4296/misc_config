;;; lang/jg-python/config.el -*- lexical-binding: t; -*-

(dlog! "Config JG Python")

(local-load! "+defs")
(local-load! "+vars")
(local-load! "+extra")

(defer-load! "+envs" "+lsp" "+cython")

(defer-load! jg-bindings-total "+bindings")

(advice-add 'python-shell-calculate-command :override    #'+jg-python-shell-calculate-command)
(advice-add 'py--pdbtrack-get-source-buffer :override    #'+jg-python-pdbtrack-silence)
(advice-add 'py--pdbtrack-track-stack-file  :override    #'+jg-python-py--pdbtrack-track-stack-file)
(advice-add 'python-ts-mode                 :around      #'+jg-python-override-python-ts)
(advice-add 'python-ts-mode                 :after       #'python-ts-extend)
(advice-add 'python-pytest--run             :filter-args #'+jg-python-test-extra-args)

(use-package! python
  :config
  ;; internal python uses prefix: python-
  (defalias 'python-stock-mode #'python-mode)
  (require 'python-mode)

  (add-hook! 'python-ts-mode-hook
             #'abbrev-mode
             #'evil-collection-python-set-evil-shift-width
             #'librarian-insert-minor-mode
             #'+jg-python-outline-regexp-override-hook
             #'outline-minor-mode
             #'maybe-py-test-minor-mode
             )

  (setq-hook! 'python-ts-mode-hook ;; flycheck specific
    lsp-diagnostic-filter       #'+jg-python-lsp-flycheck-filter
    flycheck-pylintrc                         '("pylint.toml" "pyproject.toml")
    flycheck-python-ruff-config               '("ruff.toml" ".ruff.toml" "pyproject.toml")
    flycheck--automatically-enabled-checkers  '(python-ruff python-coverage)
    flycheck--automatically-disabled-checkers '(python-pylint python-flake8 python-pycompile python-compile python-pyright python-mypy)
    flycheck-python-mypy-config               '("pyproject.toml")
    )
  )

(use-package! python-mode
  :after python
  :init
  ;; external python uses prefix: py-
  (setq py-complete-function #'(lambda () nil)
        py-font-lock-defaults-p           t
        py-use-font-lock-doc-face-p       t
        py-do-completion-p                    nil
        py-company-pycomplete-p               nil
        py-fast-process-p                     nil
        py-outline-minor-mode-p               nil
        py-hide-show-minor-mode-p             nil
        py-load-skeletons-p                   nil
        py-guess-py-install-directory-p       nil
        py-autopair-mode                      nil
        py--imenu-create-index-p              nil
        py-defun-use-top-level-p              nil
        py-sexp-use-expression-p              nil
        py-trailing-whitespace-smart-delete-p nil
        py-load-pymacs-p                      nil
        py-debug-p                            nil
        py-empty-comment-line-separates-paragraph-p t
        )

  (defvaralias 'python-indent-offset 'py-indent-offset)
  (defvaralias 'python-pdbtrack-activate 'py-pdbtrack-do-tracking-p)
  (defvaralias 'python-shell--interpreter 'py-python-command)
  (defvaralias 'python-shell-virtualenv-root 'py-shell-virtualenv-root)

  :config
  (defalias 'python-external-mode #'python-mode)

  ;;-- hooks

  (add-hook! 'python-mode-hook
             #'abbrev-mode
             #'doom--setq-tab-width-for-python-mode-h
             #'er/add-python-mode-expansions
             #'evil-collection-python-set-evil-shift-width
             #'librarian-insert-minor-mode
             #'maybe-py-test-minor-mode
             #'tree-sitter!
             )

  (add-hook! 'python-mode-hook :depth 100
             #'jg-python-font-lock-mod-h
             #'+jg-python-outline-regexp-override-hook
             #'outline-minor-mode
             )

  (add-hook! 'code-shy-minor-mode-hook #'+jg-python-auto-hide)

  (setq-hook! 'python-mode-hook ;; flycheck specific
    lsp-diagnostic-filter       #'+jg-python-lsp-flycheck-filter
    flycheck-pylintrc                         '("pylint.toml" "pyproject.toml")
    flycheck-python-ruff-config               '("ruff.toml" ".ruff.toml" "pyproject.toml")
    flycheck--automatically-enabled-checkers  '(python-ruff python-coverage)
    flycheck--automatically-disabled-checkers '(python-pylint python-flake8 python-pycompile python-compile python-pyright)
    flycheck-python-mypy-config               '("pyproject.toml")
    )

  (setq-hook! 'python-mode-hook
    beginning-of-defun-function #'python-nav-beginning-of-defun
    end-of-defun-function       #'python-nav-end-of-defun
    indent-line-function        #'python-indent-line
    indent-region-function      #'python-indent-region
    jg-workspaces-find-buff-fn #'+jg-python-carousel-window-fn
    tab-width                    py-indent-offset
    )

  ;;-- end hooks

)

(use-package! python-pytest
  :after python-mode
  )

(use-package! python-coverage
  :defer t
  :config

  (setq-hook! 'python-mode-hook
    python-coverage-default-file-name ".temp/coverage/coverage.xml"
    )

  )
