;;; +envs.el -*- lexical-binding: t; -*-

(doom-log "Loading Python Envs")

(use-package! pythonic
  :commands (pythonic-activate pythonic-deactivate)
  :init
  (advice-add 'pythonic-activate :after-while #'+modeline-update-env-in-all-windows-h)
  (advice-add 'pythonic-deactivate :after #'+modeline-clear-env-in-all-windows-h)
  )

(use-package! pipenv
  :commands (pipenv-project-p pipenv-activate pipenv-deactivate)
  :init
  (setq pipenv-with-projectile nil)
  )

(use-package! pyvenv
  :commands (pyvenv-activate pyvenv-deactivate)
  :init
  (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
  (add-hook 'pyvenv-pre-deactivate-hooks #'+modeline-clear-env-in-all-windows-h)

  )

(use-package! conda
  :commands (conda-env-activate conda-env-deactivate conda-env-read-name)
  :init
  (advice-add 'conda--get-path-prefix :override #'+jg-python-conda-get-path-prefix)

)

(use-package! micromamba
  :commands (micromamba-activate micromamba-deactivate)
  )

(use-package! poetry
  :commands (poetry-venv-workon poetry-venv-deactivate poetry-update poetry-add)

  )

(use-package! pip-requirements
  :commands pip-requirements-mode
  :config
  ;; HACK `pip-requirements-mode' performs a sudden HTTP request to
  ;;   https://pypi.org/simple, which causes unexpected hangs (see #5998). This
  ;;   advice defers this behavior until the first time completion is invoked.
  ;; REVIEW More sensible behavior should be PRed upstream.
  (advice-add 'pip-requirements-complete-at-point :before #'+python--init-completion-a)
  (advice-add 'pip-requirements-mode              :around #'+python--inhibit-pip-requirements-fetch-packages-a)
)
