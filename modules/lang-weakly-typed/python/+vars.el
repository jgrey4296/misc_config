;;; lang/jg-python/+vars.el -*- lexical-binding: t; -*-

(defvar +python-ipython-command '("ipython" "-i" "--simple-prompt" "--no-color-info")
  "Command to initialize the ipython REPL for `+python/open-ipython-repl'.")

(defvar +python-jupyter-command '("jupyter" "console" "--simple-prompt")
  "Command to initialize the jupyter REPL for `+python/open-jupyter-repl'.")

;;-- personal vars

(defvar jg-python-dev-mode nil)

(defvar jg-python-dev-cmd "-X dev")

(defvar jg-python-docs-url           "https://docs.python.org/3/")

(defvar jg-python-lib-url-suffix     "library/%s.html")

(defvar jg-conda-activate-cmd        "source $HOME/.doom.d/terminal/bash/conda.bash && activate %s")

(defvar jg-python-last-chosen-support nil)

(defvar jg-python-import-block-end-re "^\\(__all__\\|[[:graph:]]+?\\s-+=\\|def\\|class\\|if TYPE_CHECKING:\\)")

(defvar jg-python-summary-buffer      "*Python-Summary*")
(setq expand-region-preferred-python-mode 'python-mode)
;;-- end personal vars

;;-- general python
(after! python-mode
  (setq-default py-indent-offset 4 python-indent-offset 4
                py-shell-virtualenv-root (expand-file-name  "~/anaconda") python-shell-virtualenv-root (expand-file-name  "~/anaconda")
                py-pdbtrack-do-tracking-p t python-pdbtrack-activate t

                py-python-command "python3" python-shell-interpreter "python3"
                py-python-command-args '("-i") python-shell-interpreter-args "-i"
                ;; python-shell-interpreter-args `("-X" ,(format "pycache_prefix=%s" (expand-file-name  "~/.pycache")))
                jg-python-repl-start-file (doom-module-expand-path :lang-weakly-typed 'python "repl/repl_startup.py ")

                py-use-font-lock-doc-face-p t
                py-fontify-shell-buffer-p t

                python-indent-guess-indent-offset nil
                python-shell-completion-native-enable nil
                python-shell-completion-native-disabled-interpreters '("pypy")

                ;; python-shell-interpreter "python3"
                ;; python-shell-interpreter-args `("-X" ,(format "pycache_prefix=%s" (expand-file-name  "~/.pycache")))
                python-shell-interpreter-path-args (doom-module-expand-path :lang-weakly-typed 'python "repl/repl_startup.py ")
                )
  (modify-syntax-entry ?_ "_" python-mode-syntax-table)
)
;;-- end general python

;;-- outline
(after! python-mode
  (setq jg-python-outline-regexp
        (rx-let ((kwds (regexp (eval (s-join "\\|" py-outline-mode-keywords))))
                 )
        (rx (* blank)
            (or "##--"
                (| "@" (+ word))
                kwds
                )
            )
        )
        jg-python-outline-end-regexp ":[^\n]*\n"
        )
)
;;-- end outline

;;-- flycheck
(after! flycheck
  (setq flycheck-pylintrc '("pylint.toml" "pyproject.toml"))
  (setq-default flycheck--automatically-enabled-checkers (-concat flycheck--automatically-enabled-checkers '(python-pylint))
                flycheck--automatically-disabled-checkers '(python-compile python-pyright python-mypy)
                )
  (push 'python-pylint flycheck-checkers)
  (push ".mypy.ini" flycheck-python-mypy-ini)
  )
;;-- end flycheck

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
  (setq sp-python-insert-colon-in-function-definitions nil))
;;-- end smartparens

;;-- lsp

(defvar lsp-disabled-clients nil)
(add-to-list 'lsp-disabled-clients 'pyls)
(add-to-list 'lsp-disabled-clients 'pylsp)
(add-to-list 'lsp-disabled-clients 'mspyls)

(setq lsp-pyright-extra-paths #'[]
      lsp-pyright-venv-path   (list (expand-file-name "~/anaconda"))

      )
;;-- end lsp

;;-- jg-company
(setq jg-python-company-activation (rx (| "error" "lib"))
      jg-python-company-kws (let ((ht (make-hash-table :test 'equal)))
                              (puthash "error" +jg-python-ivy-exceptions ht)
                              (puthash "lib" '() ht)
                              ht
                              )
      )
;;-- end jg-company

;;-- specs
(after! projectile
  (pushnew! projectile-project-root-files "pyproject.toml" "requirements.txt" "setup.py")
  (pushnew! projectile-project-root-files "setup.py" "requirements.txt")
  )
(spec-handling-add! projects nil
                   '(python-poetry ("poetry.lock") :project-file "poetry.lock" :compilation-dir nil :configure nil :compile "poetry build" :test "poetry run python -m unittest discover" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(python-pipenv ("Pipfile") :project-file "Pipfile" :compilation-dir nil :configure nil :compile "pipenv run build" :test "pipenv run test" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(python-tox ("tox.ini") :project-file "tox.ini" :compilation-dir nil :configure nil :compile "tox -r --notest" :test "tox" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(python-pkg ("setup.py") :project-file "setup.py" :compilation-dir nil :configure nil :compile "python setup.py build" :test "python -m unittest discover" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(python-pip ("requirements.txt") :project-file "requirements.txt" :compilation-dir nil :configure nil :compile "python setup.py build" :test "python -m unittest discover" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(django ("manage.py") :project-file "manage.py" :compilation-dir nil :configure nil :compile "python manage.py runserver" :test "python manage.py test" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(jg-python-project ("pyproject.toml") :project-file "pyproject.toml" :configure "pip install -e %s" :test "python -m unittest discover -v -p test_*.py" :test-dir (lambda (x) (f-join x "__tests")) :test-prefix "test_" :related-files-fn #'+jg-python-related-files-fn)
                    )
(spec-handling-add! popup nil
                    '(python
                     ("^\\*pytest\\*"         :side bottom :ttl 5   :height 0.4 :quit t :select t :priority 50)
                     ("^\\*nosetests"         :size 0.4 :select nil)
                     ("^\\*Anaconda\\*"       :side bottom :ttl 5   :height 0.4 :quit t :select nil :priority 50)
                     ("^\\*anaconda-mode"     :side bottom :ttl 5   :height 0.4 :quit t :select nil :priority 50)
                     ("^\\*Python\\*"         :side right  :ttl nil :width  0.5 :quit nil :select t :priority 50)
                     ("^\\*Python-Summary\\*" :side right  :ttl nil :width  0.2 :quit t  :select nil :priority 50)
                     )
                    )
(spec-handling-add! file-templates nil
                    '(python
                     ("LICENSE$"        :trigger "__license-acab"   :mode text-mode :priority 100)
                     ("pyproject.toml$" :trigger "__pyproject"      :mode conf-toml-mode)
                     ("setup\\.cfg$"    :trigger "__setup_cfg"      :mode python-mode)
                     ("__init__\\.py$"  :trigger "__init"           :mode python-mode)
                     ("test_.+\\.py$"   :trigger "__tests"          :mode python-mode)
                     ("cli_.+\\.py$"    :trigger "__cli"            :mode python-mode)
                     ("conf\\.py$"      :trigger "__conf"           :mode python-mode)
                     ("setup\\.py$"     :trigger "__setup"          :mode python-mode)
                     ("dooter\\.py$"    :trigger "__doot"           :mode python-mode)
                     ("SConstruct"      :trigger "__sconstruct"     :mode python-mode)
                     ("SConscript"      :trigger "__sconscript"     :mode python-mode)
                     ("\\.py$"          :trigger "__"               :mode python-mode :priority -99)
                     (python-mode       :trigger "__" :priority -100)
                     )
                    )
(spec-handling-add! fold nil
                    '(python
                     :modes (python-mode)
                     :priority 25
                     :triggers (:close     +jg-python-close-class-defs
                                :close-all +jg-python-close-all-defs
                                :open      outline-toggle-children
                                :open-all  outline-show-all
                                :open-rec  outline-show-subtree
                                :toggle    outline-toggle-children
                                )
                     )
                    )
(spec-handling-add! rotate-text nil
                    '(python-mode
                     :symbols '(("True" "False"))
                     )
                    )
(spec-handling-add! company nil
                    '(python-mode (:front . #'jg-company/backend ) (:front . #'company-gtags))
                    '(anaconda-mode (:favour . #'company-anaconda))
                    )
(spec-handling-add! whitespace-cleanup t
                    '(python-mode
                      +jg-python-cleanup-ensure-newline-before-def
                      delete-trailing-whitespace
                      +jg-text-cleanup-whitespace
                     )
                    )
(spec-handling-add! lookup-handler nil
                    '(anaconda-mode
                      :definition +jg-conda-find-defs
                      :references +jg-conda-find-references
                      :documentation +jg-conda-show-doc)
                    )
(spec-handling-add! lookup-url nil
                    '(python
                     ("Python" "https://docs.python.org/3/search.html?q=%s&check_keywords=yes&area=default")
                     ("Pypi"   "https://pypi.org/search/?q=%s")
                     )
                    )
(spec-handling-add! lookup-regular nil
                    '(python-mode
                      ("Python" . "https://docs.python.org/3/")
                      ("Pypi"   "https://pypi.org/")
                      ("Bibtex Parser" . "https://bibtexparser.readthedocs.io/en/master/tutorial.html")
                      ("Beautiful Soup" .  "https://beautiful-soup-4.readthedocs.io/en/latest/")
                      ("Pyparsing" . "https://pyparsing-docs.readthedocs.io/en/latest/")
                      ("PyRight" . "https://github.com/Microsoft/pyright")
                      ("Z3" . "https://github.com/Z3Prover/z3")
                      ("Sorobn" . "https://github.com/MaxHalford/sorobn")
                      ("Pomegranate" . "https://pomegranate.readthedocs.io/en/latest/")
                      ("Pgmpy" . "https://pgmpy.org/")
                      ("Rich" . "https://rich.readthedocs.io/en/stable/introduction.html")
                      ("Flowweaver" . "https://floweaver.readthedocs.io/en/latest/")
                      ("Matplotlib" . "https://matplotlib.org/stable/contents.html")
                      ("Numpy" . "https://numpy.org/doc/stable/")
                      ("Pandas" . "https://pandas.pydata.org/docs/")
                      ("Seaborn" . "https://seaborn.pydata.org/api.html")
                      ("Pony" . "https://github.com/ponyorm/pony/")
                      ("Marshmallow" . "https://marshmallow.readthedocs.io/en/stable/")
                      ("Pylightxl" . "https://github.com/PydPiper/pylightxl")
                      ("pyexcel" . "https://github.com/pyexcel/pyexcel")
                      ("Setuptools" . "https://setuptools.readthedocs.io/en/latest/userguide/index.html")
                      ("Conda" . "https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html")
                      ("Poetry" . "https://python-poetry.org/")
                      ("Pip" . "https://pip.pypa.io/en/stable/")
                      ("Scons" . "https://scons.org/documentation.html")
                      ("Build" . "https://github.com/pypa/build")
                      ("Venv" . "https://docs.python.org/3/library/venv.html")
                      ("Doit" . "https://pydoit.org/contents.html")
                      ("Construct" . "https://construct.readthedocs.io/en/latest/")
                      ("Python/C api" . "https://docs.python.org/3/c-api/index.html")
                      ("Pathlib" . "https://docs.python.org/3/library/pathlib.html")
                      ("itertools" . "https://docs.python.org/3/library/itertools.html")
                      ("functools" . "https://docs.python.org/3/library/functools.html")
                      ("zlib" . "https://docs.python.org/3/library/zlib.html")
                      ("string" . "https://docs.python.org/3/library/string.html")
                      ("re" . "https://docs.python.org/3/library/re.html")
                     )
                    '(conf-mode
                     ("Setuptools" . "https://setuptools.readthedocs.io/en/latest/userguide/index.html")
                     )
                    '(manifest-mode
                     ("Manifest format" . "https://docs.python.org/3/distutils/sourcedist.html?highlight=manifest")
                     )
                    )
;;-- end specs
