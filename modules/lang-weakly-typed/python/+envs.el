;;; +envs.el -*- lexical-binding: t; -*-

(dlog! "Loading Python Envs")

(use-package! pythonic
  :commands (pythonic-activate pythonic-deactivate)
  :init
  (advice-add 'pythonic-activate :after-while #'+modeline-update-env-in-all-windows-h)
  (advice-add 'pythonic-deactivate :after #'+modeline-clear-env-in-all-windows-h)
  )

(use-package! pyvenv
  :commands (pyvenv-activate pyvenv-deactivate)
  :init
  (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
  (add-hook 'pyvenv-pre-deactivate-hooks #'+modeline-clear-env-in-all-windows-h)

  (speckler-add! lib-env ()
    :override t
    `(venv
      :lang 'python
      :start #'jg-py-venv-start
      :stop  #'jg-py-venv-stop
      :modeline #'(lambda (state &rest args) "venv")
      )
    )
  )

(use-package! micromamba
  :commands (micromamba-activate micromamba-deactivate)
  :init

  (speckler-add! lib-env ()
    :override t
    `(mamba
      :lang 'python
      :start #'jg-py-mamba-start-env
      :stop  #'jg-py-mamba-stop-env
      :modeline #'(lambda (state &rest args) (format "M:%s" (car-safe args)))
      )
    )
  )

(use-package! poetry
  :commands (poetry-venv-workon poetry-venv-deactivate poetry-update poetry-add)
  :init
  (speckler-add! lib-env ()
    `(poetry
      :start #'jg-py-poetry-start-env
      :stop  #'jg-py-poetry-stop-env
      )
    )
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

(speckler-add! lib-env ()
  :override t
  `(mypy
    :lang 'python
    :start #'jg-python-update-mypy-path
    :stop  #'(lambda (state &rest rest) (setenv "MYPYPATH" nil))
    :modeline #'(lambda (state &rest rest) "mypy ")
    )
  )
