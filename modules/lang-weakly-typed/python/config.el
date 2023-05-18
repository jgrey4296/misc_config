;;; lang/jg-python/config.el -*- lexical-binding: t; -*-

(doom-log "Config JG Python")

(load! "+vars")
(load! "+funcs")
(load! "modes/+manifest-mode")
(after! jg-bindings-total
  (load! "+bindings")
  )
(load! "+advice")
(load! "modes/+derived-modes")

(use-package! python
  :config
  (require 'python-mode)
  )

(use-package! python-mode
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'"   . conf-mode)
  :init
  (when (executable-find "Microsoft.Python.LanguageServer")
    (set-eglot-client! 'python-mode '("Microsoft.Python.LanguageServer")))

  (setq py-complete-function #'(lambda () nil)
        py-do-completion-p nil ;; nil
        py-company-pycomplete-p nil
        py-fast-process-p nil)
  :config
  ;;-- hooks
  (add-hook! 'python-mode-hook
             #'+python-use-correct-flycheck-executables-h
             #'er/add-python-mode-expansions
             #'evil-collection-python-set-evil-shift-width
             #'doom--setq-tab-width-for-python-mode-h
             #'tree-sitter!
             )

  ;; Always add auto-hide as the last thing
  (add-hook! 'python-mode-hook :depth 100
             #'+jg-python-outline-regexp-override-hook
             #'+jg-python-auto-hide
             )

  (setq-hook! 'python-mode-hook
    tab-width                    py-indent-offset
    end-of-defun-function       #'python-nav-end-of-defun
    beginning-of-defun-function #'python-nav-beginning-of-defun

    indent-line-function        #'py-indent-line
    indent-region-function      #'py-indent-region
    jg-company-activation-re    jg-python-company-activation
    jg-company-kws              jg-python-company-kws
    )
  ;;-- end hooks

)

(use-package! anaconda-mode
  :commands (anaconda-mode anaconda-mode-stop)
  :preface
  (setq anaconda-mode-installation-directory (concat doom-data-dir "anaconda/")
        anaconda-mode-eldoc-as-single-line t)
  (spec-handling-add! python-env nil
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

(use-package! company-anaconda
  :commands 'company-anaconda
  )

;;-- lsp

(use-package! lsp-pyright
  :after lsp-mode
  :when (modulep! +lsp)
  :preface
  (spec-handling-add! python-env nil
                      `(pyright
                        (:support pyright
                                  ,#'(lambda (state)
                                       (when (plist-get state :name)
                                         (setq lsp-pyright-extra-paths (vector python-shell-extra-pythonpaths
                                                                               (f-join (plist-get state :path)
                                                                                       (plist-get state :name)))))
                                       (add-hook 'python-mode-hook #'lsp-deferred)
                                       )
                                  ,#'(lambda (state)
                                       (when (and (boundp 'lsp-mode) lsp-mode)
                                         (lsp-mode -1))
                                       (when (fboundp 'lsp--last-active-workspaces)
                                         (lsp-workspace-shutdown (car lsp--last-active-workspaces)))
                                       (remove-hook 'python-mode-hook #'lsp-deferred)
                                       (setq lsp-pyright-extra-paths #'[])
                                       )
                                  )
                        (:teardown pyright ,#'(lambda (state) (lsp-disconnect)))
                        )
                      )
  )

(use-package! lsp-python-ms
  :disabled
  :unless (modulep! +pyright)
  :after (python lsp-mode)
  :init
  (add-to-list 'lsp-disabled-clients 'python-ms)
  :config
  (setq lsp-python-ms-python-executable-cmd python-shell-interpreter)
  )

(use-package! lsp-jedi
  :disabled
  :ensure t
  :after lsp-mode
  :init
  (add-to-list 'lsp-disabled-clients 'jedi)
  )

;;-- end lsp

;;-- tests

(use-package! pyimport :after python-mode)

(use-package! py-isort
  :defer t
  :init
  (map! :after python
        :map python-mode-map
        :localleader
        (:prefix ("i" . "imports")
          :desc "Sort imports"      "s" #'py-isort-buffer
          :desc "Sort region"       "r" #'py-isort-region))
  )

(use-package! nose
  :commands nose-mode
  :preface

(defvar nose-mode-map (make-sparse-keymap))
  :minor ("/test_.+\\.py$" . nose-mode)
  :config
  (set-yas-minor-mode! 'nose-mode)
  (when (featurep 'evil)
    (add-hook 'nose-mode-hook #'evil-normalize-keymaps))
  )

(use-package! python-pytest
  :commands python-pytest-dispatch
)

;;-- end tests

;;-- envs
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
;;-- end envs

;;-- cython

(use-package! cython-mode
  :when (modulep! +cython)
  :mode "\\.p\\(yx\\|x[di]\\)\\'"
  :config
  (setq cython-default-compile-format "cython -a %s")
)

(use-package! flycheck-cython
  :when (modulep! +cython)
  :when (modulep! :checkers syntax)
  :after cython-mode)

;;-- end cython

;; (after! (origami python-origami)
;; (load! "util/jg-python-origami")
 ;;  (delq (assoc 'python-mode origami-parser-alist) origami-parser-alist)
 ;;  (add-to-list 'origami-parser-alist '(python-mode . +jg-origami-python-parser))
 ;;  )

(spec-handling-new! python-env env-handling-registered nil append
                    val
                    )
