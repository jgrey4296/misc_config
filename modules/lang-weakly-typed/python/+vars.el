;;; lang/jg-python/+vars.el -*- lexical-binding: t; -*-

(dlog! "Python Vars")
;;-- general python

;; Conda/Mamaba
(spec-handling-setq! conda 50
                     conda-anaconda-home (or (getenv "MAMBA_ROOT_PREFIX") (getenv "ANACONDA_HOME") "/usr/local/anaconda3")
                     conda-env-home-directory (cond ((getenv "MAMBA_ROOT_PREFIX")
                                                     (f-join (getenv "MAMBA_ROOT_PREFIX") "envs"))
                                                    ((getenv "ANACONDA_ENVS")
                                                     (getenv "ANACONDA_ENVS"))
                                                    (t
                                                     (f-join conda-anaconda-home "envs"))
                                                    )
                     )

;; Py-vars
(spec-handling-setq! python 50
                     ;; Python settings
                     python-indent-offset 4
                     python-indent-guess-indent-offset                     nil
                     python-shell-completion-native-enable                 nil
                     python-shell-completion-native-disabled-interpreters  '("pypy")
                     python-shell-interpreter-path-args                    (expand-file-name "python/repl_startup.py"  templates-loc)
                     expand-region-preferred-python-mode 'python-mode
                     ;; py settings
                     py-shell-virtualenv-root      conda-env-home-directory
                     py-pdbtrack-do-tracking-p     t
                     py-python-command             "python3"
                     py-python-command-args        '("-i")
                     py-use-font-lock-doc-face-p   t
                     py-fontify-shell-buffer-p     t
                     py-split-window-on-execute    t
                     ;; my settings
                     jg-python-current-interpreter                         jg-python-stock-repl
                     jg-python-repl-start-file (expand-file-name "python/repl_startup.py " templates-loc)
                     jg-python-coverage-file-loc ".temp/coverage"
                     )
(modify-syntax-entry ?_ "_" python-mode-syntax-table)
;;-- end general python

;;-- tree-sitter
(spec-handling-add! treesit-lang
                    '(python :lib-base "python" :entry-func "tree_sitter_python")
                    )

;;-- end tree-sitter

;;-- outline
(after! python-mode
    (rx-let ((kwds (regexp (eval (s-join "\\|" py-outline-mode-keywords))))
            )
    (setq jg-python-outline-regexp
            (rx (* blank)
                (or "##--"
                    (| "@" (+ word))
                    kwds
                    )
                )
            jg-python-outline-end-regexp ":[^\n]*\n"
            )
    )
)
;;-- end outline

;;-- smartparens
(after! smartparens-python
  (sp-with-modes 'python-mode
    ;; Automatically close f-strings
    (sp-local-pair "f\"" "\"")
    (sp-local-pair "f\"\"\"" "\"\"\"")
    (sp-local-pair "f'''" "'''")
    (sp-local-pair "f'" "'"))
  ;; Original keybind interferes with smartparens rules
  (define-key python-mode-map (kbd "DEL") nil)
  ;; Interferes with the def snippet in doom-snippets
  ;; TODO Fix this upstream, in doom-snippets, instead
  (setq sp-python-insert-colon-in-function-definitions nil)
  )
;;-- end smartparens

;;-- lsp
(spec-handling-setq! lsp-py 50
                     ;; Pyright
                     lsp-pyright-extra-paths #'[]
                     lsp-pyright-venv-path conda-env-home-directory
                     lsp-pyright-log-level "Information" ;; Error Warning Information Trace
                     lsp-pyright-typechecking-mode "basic"
                     ;; Ruff
                     lsp-ruff-lsp-advertize-fix-all            nil
                     lsp-ruff-lsp-advertize-organize-imports   t
                     lsp-ruff-lsp-show-notifications           "always"
                     lsp-ruff-lsp-log-level                    "info"
                     lsp-ruff-lsp-server-command               '("ruff-lsp")
                     lsp-ruff-lsp-python-path                  "python3"
                     lsp-ruff-lsp-import-strategy              "fromEnvironment"
                     lsp-ruff-lsp-ruff-args                    [ ]
                     lsp-ruff-lsp-ruff-path                    ["ruff"]
                     ;; pylsp
                     lsp-pylsp-configuration-sources []
                     lsp-pylsp-server-command '("pylsp")
                     lsp-pylsp-plugins-flake8-enabled nil
                     )

;;-- end lsp

;;-- babel
(spec-handling-add! babel
                    '(python
                      (:name python :lib ob-python :mode python)
                      )
                    )
;; (after! (ob python)
;;   (setq org-babel-python-command
;;         (string-trim
;;          (concat python-shell-interpreter " "
;;                  (if (string-match-p "\\<i?python[23]?$" python-shell-interpreter)
;;                      (replace-regexp-in-string
;;                       "\\(^\\| \\)-i\\( \\|$\\)" " " python-shell-interpreter-args)
;;                    python-shell-interpreter-args))))
;;   )

;;-- end babel

;;-- general insert
(general-insert-register-processor 'python-mode "raise" #'(lambda (x) (insert "raise " (s-replace-regexp "^[^A-Z]+" "" x))))
(general-insert-register-processor 'python-mode "datetime" #'(lambda (x) (insert (car (split-string x " " t " +")))))
(general-insert-register-processor 'python-mode "fixtures" #'(lambda (x) (insert (car (split-string x " " t " "+)))))
(general-insert-register-processor 'python-mode "import" #'(lambda (x) (insert "import " (car (split-string x t " +")))))
(general-insert-register-processor 'python-ts-mode "raise" #'(lambda (x) (insert "raise " (s-replace-regexp "^[^A-Z]+" "" x))))
(general-insert-register-processor 'python-ts-mode "datetime" #'(lambda (x) (insert (car (split-string x " " t " +")))))
(general-insert-register-processor 'python-ts-mode "fixtures" #'(lambda (x) (insert (car (split-string x " " t " "+)))))
(general-insert-register-processor 'python-ts-mode "import" #'(lambda (x) (insert "import " (car (split-string x t " +")))))
;;-- end general insert

;;-- env handling
(spec-handling-add! env-handling ;; pythonic
                    `(pythonic
                      (:setup pythonic
                              ,#'(lambda (state local) (pythonic-activate (f-join (plist-get state :path) (plist-get state :name)))
                                   nil)
                              ,#'(lambda (state) (pythonic-deactivate))
                              )
                      )
                    )
(spec-handling-add! env-handling ;; pipenv / pip
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
(spec-handling-add! env-handling ;; venv
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
(spec-handling-add! env-handling ;; conda
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
(spec-handling-add! env-handling ;; mamba
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
(spec-handling-add! env-handling ;; poetry
                      `(poetry
                        (:setup poetry
                                ,#'(lambda (state) (poetry-venv-workon) nil)
                                ,#'(lambda (state) (poetry-venv-deactivate))
                                )
                        (:update poetry ,#'(lambda (state) (poetry-update)))
                        (:install poetry ,#'(lambda (state) (poetry-add)))
                        )
                      )
;;-- end env handling

;;-- specs
(spec-handling-add! projects
                   `(jg-python-project ("pyproject.toml") :project-file "pyproject.toml" :configure "pip install -e %s" :test "python -m unittest discover -v -p test_*.py" :test-dir (lambda (x) (f-join x "__tests")) :test-prefix "test_" :related-files-fn ,#'related-files:jg-python-project)
                   '(python-poetry ("poetry.lock") :project-file "poetry.lock" :compilation-dir nil :configure nil :compile "poetry build" :test "poetry run python -m unittest discover" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(python-pipenv ("Pipfile") :project-file "Pipfile" :compilation-dir nil :configure nil :compile "pipenv run build" :test "pipenv run test" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(python-tox ("tox.ini") :project-file "tox.ini" :compilation-dir nil :configure nil :compile "tox -r --notest" :test "tox" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(python-pkg ("setup.py") :project-file "setup.py" :compilation-dir nil :configure nil :compile "python setup.py build" :test "python -m unittest discover" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(python-pip ("requirements.txt") :project-file "requirements.txt" :compilation-dir nil :configure nil :compile "python setup.py build" :test "python -m unittest discover" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(python-basic ("setup.py") :project-file "setup.py")
                   '(django ("manage.py") :project-file "manage.py" :compilation-dir nil :configure nil :compile "python manage.py runserver" :test "python manage.py test" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                    )
(spec-handling-add! popup
                    '(python
                     ("^\\*pytest\\*"         :side bottom :ttl 5   :height 0.4 :quit t :select t :priority 50)
                     ("^\\*nosetests"         :size 0.4 :select nil)
                     ("^\\*Anaconda\\*"       :side bottom :ttl 5   :height 0.4 :quit t :select nil :priority 50)
                     ("^\\*anaconda-mode"     :side bottom :ttl 5   :height 0.4 :quit t :select nil :priority 50)
                     ("^\\*Python\\*"         :side right  :ttl nil :width  0.5 :quit nil :select t :priority 50)
                     ("^\\*Python-Summary\\*" :side right  :ttl nil :width  0.2 :quit t  :select nil :priority 50)
                     ("^\\*PyDoc Search\\*"   :side left   :ttl nil :width  0.3 :quit t  :select nil :priority 50)
                     ("^\\*pydoc\\*"          :side left   :ttl nil :width  0.3 :quit t  :select nil :priority 50)
                     ("^\\*python-dis\\*"     :side right  :ttl 5   :width  0.4 :quit t  :select nil :priority 50)
                     ("^\\*imports\\*"        :side bottom :ttl nil :height -1.3 :quit t  :select nil :priority 50)
                     )
                    )
(spec-handling-add! file-templates
                    '(python
                      ("LICENSE\\'"               :trigger "__license-acab"   :mode text-mode   :priority 100)
                      ;;Configs:
                      ("pyproject.toml\\'"        :trigger "__pyproject"      :mode conf-toml-mode)
                      ("pyrightconfig.json\\'"    :trigger "__pyrightconfig"  :mode json-mode)
                      ("pylint.toml\\'"           :trigger "__pylint"         :mode conf-toml-mode)
                      ("jekyl.toml\\'"            :trigger "__jekyll"         :mode conf-toml-mode)
                      ("ruff.toml\\'"             :trigger "__ruff_config"    :mode conf-toml-mode)
                      ("\\.mypyrc\\'"               :trigger "__mypy"    :mode conf-toml-mode)
                      ("conf.py"               :trigger "__sphinx_conf"    :mode python-mode)
                      ("log_config.py"         :trigger "__log_config"     :mode python-mode)

                      ;; Python:
                      ("__init__\\.py\\'"      :trigger "__init"           :mode python-mode)
                      ("test_.+\\.py\\'"       :trigger "__pytest"         :mode python-mode)
                      ("cli_.+\\.py\\'"        :trigger "__cli"            :mode python-mode)
                      ("\\.py\\'"              :trigger "__"               :mode python-mode :priority -99)
                      (python-mode             :trigger "__"                                 :priority -100)
                      )
                    )
(spec-handling-add! fold
                    `(python
                     :modes python-mode
                     :priority 25
                     :triggers (:close     ,#'+jg-python-close-class-defs
                                :close-all ,#'+jg-python-close-all-defs
                                :open      ,#'outline-toggle-children
                                :open-all  ,#'outline-show-all
                                :open-rec  ,#'outline-show-subtree
                                :toggle    ,#'outline-toggle-children
                                )
                     )
                    `(python-ts
                     :modes python-ts-mode
                     :priority 25
                     :triggers (:close     ,#'hs-hide-block-at-point
                                :close-all ,#'hs-hide-all
                                :open      ,#'hs-show-block
                                :open-all  ,#'hs-show-all
                                :open-rec  ,#'hs-show-all
                                :toggle    ,#'hs-toggle-hiding
                                )
                      )
                    )
(spec-handling-add! rotate-text
                    '(python-mode
                      :symbols (("True" "False")
                                ("dict" "list")
                                )
                      )
                    )
(spec-handling-add! whitespace-cleanup
                    `((python-mode python-ts-mode)
                      ,#'+jg-python-cleanup-ensure-newline-before-def
                      ,#'delete-trailing-whitespace
                      ,#'+jg-text-cleanup-whitespace
                     )
                    )
(spec-handling-add! modeline
                    '(python
                      (env-handling-state (:eval (env-handling-state-line)))
                      )
                    )
(spec-handling-add! env-handling
                    '(default (:setup  none) (:support none))
                    )
(spec-handling-add! ligatures
                    '(python-mode
                      ;; Functional
                      "def"    :def
                      "lambda" :lambda
                      ;; Types
                      "None" :null
                      "True" :true "False" :false
                      "int"  :int "str" :str "float" :float "bool" :bool "tuple" :tuple
                      ;; Flow
                      "not"    :not
                      "in"     :in
                      "not in" :not-in
                      "and"    :and "or" :or
                      "for"    :for
                      "return" :return
                      "yield"  :yield
                      )
                    )
(spec-handling-add! tree-sit-lang
                    '(python-mode . python)
                    '(doot-mode . python)
                    )
(spec-handling-add! lookup-url
                    '(python
                     ("Python" "https://docs.python.org/3/search.html?q=%s&check_keywords=yes&area=default")
                     ("Pypi"   "https://pypi.org/search/?q=%s")
                     )
                    )
(spec-handling-add! lookup-handler
                    `(anaconda-mode
                      :definition    +jg-conda-find-defs
                      :references    +jg-conda-find-references
                      :documentation +jg-conda-show-doc
                      :assignments   +jg-conda-find-assignments
                      )
                    )
(spec-handling-add! company
                    '(python-mode (:mode company-gtags))
                    '(anaconda-mode (:mode company-anaconda))
                    )
(spec-handling-add! docsets
                    '((python-mode inferior-python-mode)
                      "Python 3" "NumPy" "SciPy" "Pandas"
                      )
                    )
(spec-handling-add! auto-modes
                    '(python
                      ("\\.py\\'"                 . python-mode)
                      ("SConscript"               . scons-mode)
                      ("SConstruct"               . scons-mode)
                      ("dooter\\.py"              . doit-mode)
                      ("[./]flake8\\'"            . conf-mode)
                      ("/Pipfile\\'"              . conf-mode)
                      ("MANIFEST.in"              . manifest-mode)
                      ("\\.p\\(yx\\|x[di]\\)\\'"  . cython-mode)
                      ("pyproject\\.toml\\'"      . conf-toml-mode)
                    )
                    )
(spec-handling-add! compile-commands
                    '(python +jg-python-get-commands +jg-python-solo-file-run +jg-python-distribute-commands)
                    )
(spec-handling-add! repl :form 'override
                    '(python-mode
                      :start +jg-python/open-repl
                      :send  python-shell-send-region
                      )
                    )
(spec-handling-add! yas-extra
                    '(node-mode node-mode)
                    )
(when (executable-find "Microsoft.Python.LanguageServer")
  (spec-handling-add! eglot
                      '(python-mode "Microsoft.Python.LanguageServer")
                      )
  )
(spec-handling-add! imenu
                    '(python-mode
                      ("Field"  "\\s-*\\(.+?\\)\\s-*:\\s-\\(.+?\\)\\s-=\\s-field" 1)

                      )
                    )

(spec-handling-add! org-src
                    '(python
                      ("python" . python)
                      )
                    )
(spec-handling-add! babel
                    '(python
                      (:name python :lib ob-python)
                      )
                    )
;;-- end specs
