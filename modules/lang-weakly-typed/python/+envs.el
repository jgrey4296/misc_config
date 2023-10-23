;;; +envs.el -*- lexical-binding: t; -*-

(doom-log "Loading Python Envs")

(use-package! pythonic
  :commands (pythonic-activate pythonic-deactivate)
  :init
  (spec-handling-add! python-env
                      `(pythonic
                        (:setup pythonic
                                ,#'(lambda (state local) (pythonic-activate (f-join (plist-get state :path) (plist-get state :name)))
                                     nil)
                                ,#'(lambda (state) (pythonic-deactivate))
                                )
                        )
                      )
  (advice-add 'pythonic-activate :after-while #'+modeline-update-env-in-all-windows-h)
  (advice-add 'pythonic-deactivate :after #'+modeline-clear-env-in-all-windows-h)
  )

(use-package! pipenv
  :commands (pipenv-project-p pipenv-activate pipenv-deactivate)
  :init
  (setq pipenv-with-projectile nil)
  (spec-handling-add! python-env
                      `(pipenv
                        (:setup pipenv
                                ,#'(lambda (state local) (pipenv-activate) nil)
                                ,#'(lambda (state) (pipenv-deactivate))
                                )
                        (:install pipenv
                                  ,#'(lambda (state)
                                       (apply 'start-process env-handling-process-name env-handling-buffer-name "pipenv" "--non-interactive" "install"
                                              (split-string (read-string "Packages: ") " " t t)))
                                  )
                        (:update pipenv
                                 ,#'(lambda (state)
                                      (apply 'start-process env-handling-process-name env-handling-buffer-name "pipenv" "--non-interactive" "upgrade" ))
                                 )
                        )
                      `(pip
                        (:install pip
                                  ,#'(lambda (state)
                                    (apply 'start-process env-handling-process-name env-handling-buffer-name "pip" "--no-input" "install"
                                           (split-string (read-string "Packages: ") " " t t)))
                                  )
                        (:update pip
                                 ,#'(lambda (state)
                                      (apply 'start-process env-handling-process-name env-handling-buffer-name "pip" "--no-input" "install" "--upgrade" ))
                                 )
                        )
                      )
  )

(use-package! pyvenv
  :commands (pyvenv-activate pyvenv-deactivate)
  :init
  (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
  (add-hook 'pyvenv-pre-deactivate-hooks #'+modeline-clear-env-in-all-windows-h)
  (spec-handling-add! python-env
                      `(venv
                        (:setup venv
                                ,#'(lambda (state local) (pyvenv-activate (f-join
                                                                           (plist-get state :path)
                                                                           (plist-get state :name)))
                                     nil)
                                ,#'(lambda (state) (pyvenv-deactivate))
                                )
                        (:create venv ,#'(lambda (state) (pyvenv-create)))
                        )
                      )
  )

(use-package! conda
  :commands (conda-env-activate conda-env-deactivate conda-env-read-name)
  :init
  (spec-handling-add! python-env
                      `(conda_el
                        (:setup conda
                                ,#'(lambda (state local)
                                     (let ((env-name (or (plist-get local :name)
                                                         (plist-get state :name)
                                                         (string-trim (conda-env-read-name "Select Conda Environment: ")))))
                                       (conda-env-activate env-name)
                                       (setenv "CONDA_DEFAULT_ENV" env-name)
                                       (list :name env-name :path conda-env-home-directory)
                                       )
                                   )
                                ,#'(lambda (state)
                                    (conda-env-deactivate)
                                    (setenv "CONDA_DEFAULT_ENV" nil)
                                    )
                                )
                        (:create conda
                                 ,#'(lambda (state)
                                      (let ((name (read-string "Env name to create: "))
                                            (ver  (format "python=%s" (read-string "Python Version: " "3.11")))
                                            (packages (split-string (read-string "Packages: ") " " t " +"))
                                            )
                                        (apply 'start-process env-handling-process-name env-handling-buffer-name "conda" "create" "--yes" "-n" name ver packages)))
                                 )
                        (:install conda
                                  ,#'(lambda (state)
                                      (apply 'start-process env-handling-process-name env-handling-buffer-name "conda" "install" "--yes"
                                             (split-string (read-string "Packages: ") " " t t)))
                                  )
                        (:update conda
                                 ,#'(lambda (state) (apply 'start-process env-handling-process-name env-handling-buffer-name "conda" "update" "--all" "--yes"))
                                 )
                        )
                      )
)

(use-package! micromamba
  :commands (micromamba-activate micromamba-deactivate)
  :init
  (spec-handling-add! python-env
                      `(mamba
                        (:setup mamba
                                ,#'(lambda (state local)
                                     (let ((env-name (or (plist-get local :name)
                                                         (plist-get state :name)
                                                         (string-trim (read-string "Select Environment: ")))))
                                       (micromamba-activate env-name)
                                       (setenv "CONDA_DEFAULT_ENV" env-name)
                                       (list :name env-name :path conda-env-home-directory)
                                       )
                                     )
                                ,#'(lambda (state)
                                     (micromamba-deactivate)
                                     (setenv "CONDA_DEFAULT_ENV" nil)
                                     )
                                )
                        )
                      )
  )

(use-package! poetry
  :commands (poetry-venv-workon poetry-venv-deactivate poetry-update poetry-add)
  :init
  (spec-handling-add! python-env
                      `(poetry
                        (:setup poetry
                                ,#'(lambda (state) (poetry-venv-workon) nil)
                                ,#'(lambda (state) (poetry-venv-deactivate))
                                )
                        (:update poetry ,#'(lambda (state) (poetry-update)))
                        (:install poetry ,#'(lambda (state) (poetry-add)))
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
