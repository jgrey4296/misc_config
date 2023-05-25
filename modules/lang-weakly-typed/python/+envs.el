;;; +envs.el -*- lexical-binding: t; -*-

(doom-log "Loading Python Envs")

(use-package! pythonic
  :defer t
  :init
  (spec-handling-add! python-env nil
                      `(pythonic
                        (:setup pythonic
                                ,#'(lambda (state local) (pythonic-activate (f-join (plist-get state :path) (plist-get state :name)))
                                     nil)
                                ,#'(lambda (state) (pythonic-deactivate))
                                )
                        )
                      )
  (advice-add #'pythonic-activate :after-while #'+modeline-update-env-in-all-windows-h)
  (advice-add #'pythonic-deactivate :after #'+modeline-clear-env-in-all-windows-h)
  )

(use-package! pipenv
  :commands pipenv-project-p
  :init
  (setq pipenv-with-projectile nil)
  (spec-handling-add! python-env nil
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
  :after python
  :init
  (when (modulep! :ui modeline)
    (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
    (add-hook 'pyvenv-pre-deactivate-hooks #'+modeline-clear-env-in-all-windows-h))
  (spec-handling-add! python-env nil
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
  :when (modulep! +conda)
  :after python
  :init
  (spec-handling-add! python-env t
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
  :config

)

(use-package! poetry
  :after python
  :init
  (spec-handling-add! python-env nil
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
  :defer t
  :config
  ;; HACK `pip-requirements-mode' performs a sudden HTTP request to
  ;;   https://pypi.org/simple, which causes unexpected hangs (see #5998). This
  ;;   advice defers this behavior until the first time completion is invoked.
  ;; REVIEW More sensible behavior should be PRed upstream.

(defadvice! +python--init-completion-a (&rest args)
    "Call `pip-requirements-fetch-packages' first time completion is invoked."
    :before #'pip-requirements-complete-at-point
    (unless pip-packages (pip-requirements-fetch-packages)))

(defadvice! +python--inhibit-pip-requirements-fetch-packages-a (fn &rest args)
    "No-op `pip-requirements-fetch-packages', which can be expensive."
    :around #'pip-requirements-mode
    (letf! ((#'pip-requirements-fetch-packages #'ignore))
      (apply fn args))))
