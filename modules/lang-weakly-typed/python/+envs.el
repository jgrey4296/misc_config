;;; +envs.el -*- lexical-binding: t; -*-

(dlog! "Loading Python Envs")

(use-package! pythonic
  :commands (pythonic-activate pythonic-deactivate)
  :init
  (advice-add 'pythonic-activate :after-while #'+modeline-update-env-in-all-windows-h)
  (advice-add 'pythonic-deactivate :after #'+modeline-clear-env-in-all-windows-h)
  (speckler-add! lib-env ()
    `(pythonic
      :start #'(lambda (state &rest rest) (pythonic-activate (car rest)))
      :stop  #'(lambda (state &rest rest) (pythonic-deactivate))
      )
    )
  )

(use-package! pipenv
  :commands (pipenv-project-p pipenv-activate pipenv-deactivate)
  :init
  (setq pipenv-with-projectile nil)
  (speckler-add! lib-env ()
    `(pipenv
      :lang python
      :start #'(lambda (state &rest rest)
                 (jg-py--enter-env-update-paths (librarian--envs-loc-root (librarian--envs-state-loc state)))
                 (pipenv-activate))
      :stop  #'(lambda (state &rest rest)
                 (jg-py--exit-env-update-paths (librarian--envs-loc-root (librarian--envs-state-loc state)))
                 (pipenv-deactivate))
      )
    )
  )

(use-package! pyvenv
  :commands (pyvenv-activate pyvenv-deactivate)
  :init
  (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
  (add-hook 'pyvenv-pre-deactivate-hooks #'+modeline-clear-env-in-all-windows-h)
  (speckler-add! lib-env ()
    `(venv
      :start #'(lambda (state &rest rest)
                 (jg-py--enter-env-update-paths (librarian--envs-loc-root (librarian--envs-state-loc state)))
                 (pyvenv-activate (car rest)))
      :stop  #'(lambda (state &rest rest)
                 (jg-py--exit-env-update-paths (librarian--envs-loc-root (librarian--envs-state-loc state)))
                 (pyvenv-deactivate)
                 )
      )
    )
  )

(use-package! conda
  :commands (conda-env-activate conda-env-deactivate conda-env-read-name)
  :init
  (advice-add 'conda--get-path-prefix :override #'+jg-python-conda-get-path-prefix)
  (speckler-add! lib-env ()
    `(conda
      :start #'(lambda (state &rest rest)
                 (conda-env-activate (car rest))
                 (setenv "CONDA_DEFAULT_ENV" (car rest))
                 )
      :stop #'(lambda (state &rest rest)
                (conda-env-deactivate)
                (setenv "CONDA_DEFAULT_ENV" nil))
      )
    )

  )

(use-package! micromamba
  :commands (micromamba-activate micromamba-deactivate)
  :init
  (speckler-add! lib-env ()
    `(mamba
      :start #'(lambda (state &rest rest)
                 (jg-py--enter-env-update-paths (librarian--envs-loc-root (librarian--envs-state-loc state)))
                 (micromamba-activate (car rest))
                 (setenv "CONDA_DEFAULT_ENV" (car rest))
                 )
      :stop #'(lambda (state &rest rest)
                (jg-py--exit-env-update-paths (librarian--envs-loc-root (librarian--envs-state-loc state)))
                (micromamba-deactivate)
                (setenv "CONDA_DEFAULT_ENV" nil))
      )
    )
  )

(use-package! poetry
  :commands (poetry-venv-workon poetry-venv-deactivate poetry-update poetry-add)
  :init
  (speckler-add! lib-env ()
    `(poetry
      :start #'(lambda (state &rest rest)
                 (poetry-venv-workon))
      :stop  #'(lambda (state &rest rest)
                 (poetry-venv-deactivate))
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

(defun jg-py--enter-env-update-paths (root)
  (when (boundp 'python-shell-extra-pythonpaths)
    (add-to-list 'python-shell-extra-pythonpaths root))
  (when (boundp 'py-shell-extra-pythonpaths)
    (add-to-list 'py-shell-extra-pythonpaths root))
  )

(defun jg-py--exit-env-update-paths (root)
  (when (boundp 'python-shell-extra-pythonpaths)
    (setq python-shell-extra-pythonpaths
          (remove root python-shell-extra-pythonpaths)))
  (when (boundp 'py-shell-extra-pythonpaths)
    (setq py-shell-extra-pythonpaths
          (remove root py-shell-extra-pythonpaths)))
  )
