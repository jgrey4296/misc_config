;;; lang/jg-python/config.el -*- lexical-binding: t; -*-

(dlog! "Config JG Python")

(local-load! "+spec-defs")
(local-load! "+defs")
(local-load! "+vars")
(local-load! "+extra-config")

(defer-load! "+envs" "+lsp" "+cython")

(defer-load! jg-bindings-total "+bindings")

(advice-add 'python-shell-calculate-command :override #'+jg-python-shell-calculate-command)
(advice-add 'py--pdbtrack-get-source-buffer :override #'+jg-python-pdbtrack-silence)
(advice-add 'py--pdbtrack-track-stack-file  :override #'+jg-python-py--pdbtrack-track-stack-file)

(use-package! python
  :config
  (require 'python-mode)

  )

(use-package! python-mode
  :after python
  :init
  (setq py-complete-function #'(lambda () nil)
        py-do-completion-p nil ;; nil
        py-company-pycomplete-p nil
        py-fast-process-p nil)

  (defvaralias 'python-indent-offset 'py-indent-offset)
  (defvaralias 'python-pdbtrack-activate 'py-pdbtrack-do-tracking-p)
  (defvaralias 'python-shell--interpreter 'py-python-command)
  (defvaralias 'python-shell-virtualenv-root 'py-shell-virtualenv-root)

  :config

  ;; setup python-ts-mode
  (add-hook! 'python-ts-mode-hook
             #'abbrev-mode
             #'evil-collection-python-set-evil-shift-width
             #'general-insert-minor-mode
             #'hs-minor-mode
             #'maybe-py-test-minor-mode
             #'+jg-python-auto-hide
             )

  ;;-- hooks

  (add-hook! 'python-mode-hook
             #'abbrev-mode
             #'doom--setq-tab-width-for-python-mode-h
             #'er/add-python-mode-expansions
             #'evil-collection-python-set-evil-shift-width
             #'general-insert-minor-mode
             #'maybe-py-test-minor-mode
             #'tree-sitter!
             )

  ;; Always add auto-hide as the last thing
  (add-hook! 'python-mode-hook :depth 100
             #'jg-python-font-lock-mod-h
             #'+jg-python-outline-regexp-override-hook
             #'+jg-python-auto-hide
             )

  (setq-hook! 'python-mode-hook ;; flycheck specific
    lsp-diagnostic-filter       #'+jg-python-lsp-flycheck-filter
    flycheck-pylintrc                         '("pylint.toml" "pyproject.toml")
    flycheck-python-ruff-config               '("ruff.toml" ".ruff.toml" "pyproject.toml")
    flycheck--automatically-enabled-checkers  '(python-ruff python-coverage)
    flycheck--automatically-disabled-checkers '(python-pylint python-flake8 python-pycompile python-compile python-pyright python-mypy)
    flycheck-python-mypy-config               '(".mypy.ini" "mypy.ini" "pyproject.toml")
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

(use-package! anaconda-mode
  :commands (anaconda-mode anaconda-mode-stop)
  :preface
  (setq anaconda-mode-installation-directory (concat doom-data-dir "anaconda/")
        anaconda-mode-eldoc-as-single-line t)
  (spec-handling-add! python-env
                      `(anaconda
                        (:support conda
                                  ,#'(lambda (state) (add-hook 'python-mode-hook #'anaconda-mode))
                                  ,#'(lambda (state) (anaconda-mode-stop) (remove-hook 'python-mode-hook #'anaconda-mode))
                                  )
                        (:teardown conda
                                   ,#'(lambda (state) (anaconda-mode-stop)
                                        (anaconda-eldoc-mode -1))
                                   )
                        )
                      )
  :config
  (add-hook! 'anaconda-mode-hook
             #'anaconda-eldoc-mode
             #'evil-normalize-keymaps
             )
)

(use-package! python-pytest
  :after python-mode
  )

(use-package! python-coverage
  :defer t
  :config

  (add-hook! 'python-mode-hook :depth 50
             #'python-coverage-overlay-mode
             )

  (setq-hook! 'python-mode-hook
    python-coverage-default-file-name ".temp/coverage/coverage.xml"
    )

  )
