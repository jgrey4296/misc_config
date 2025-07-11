;;; +envs.el -*- lexical-binding: t; -*-

(dlog! "Loading Python Envs")

(use-package! pyvenv
  ;; dependencies: none
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

(use-package! poetry
  ;; dependencies: pyvenv
  :commands (poetry-venv-workon poetry-venv-deactivate poetry-update poetry-add)
  :init
  (speckler-add! lib-env ()
    `(poetry
      :start #'jg-py-poetry-start-env
      :stop  #'jg-py-poetry-stop-env
      )
    )
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
