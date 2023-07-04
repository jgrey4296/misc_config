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

(defvar jg-python-import-block-end-re "^\\(__all__\\|[[:graph:]]+?\\s-+=\\|def\\|class\\|if TYPE_CHECKING:\\)")

(defvar jg-python-summary-buffer      "*Python-Summary*")

(setq expand-region-preferred-python-mode 'python-mode)
;;-- end personal vars

;;-- general python

(defvaralias 'python-indent-offset 'py-indent-offset)

(defvaralias 'python-pdbtrack-activate 'py-pdbtrack-do-tracking-p)

(defvaralias 'python-shell-interpreter 'py-python-command)

(defvaralias 'python-shell-virtualenv-root 'py-shell-virtualenv-root)

(setq py-indent-offset 4
      conda-anaconda-home (or (getenv "ANACONDA_HOME") "/usr/local/anaconda3")
      conda-env-home-directory (or (getenv "ANACONDA_ENVS") (f-join conda-anaconda-home "envs"))
      py-shell-virtualenv-root conda-env-home-directory
      lsp-pyright-venv-path conda-env-home-directory

      py-pdbtrack-do-tracking-p t

      py-python-command "python3"
      py-python-command-args '("-i")
      python-shell-interpreter-args "-i"

      py-use-font-lock-doc-face-p t
      py-fontify-shell-buffer-p t

      python-indent-guess-indent-offset nil
      python-shell-completion-native-enable nil
      python-shell-completion-native-disabled-interpreters '("pypy")

      jg-python-repl-start-file (doom-module-expand-path :lang-weakly-typed 'python "repl/repl_startup.py ")
      python-shell-interpreter-path-args (doom-module-expand-path :lang-weakly-typed 'python "repl/repl_startup.py ")
        )
        (modify-syntax-entry ?_ "_" python-mode-syntax-table)
;;-- end general python

;;-- outline
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
;;-- end outline

;;-- flycheck
(after! flycheck
  (setq flycheck-pylintrc '("pylint.toml" "pyproject.toml")
        flycheck--automatically-enabled-checkers (-concat flycheck--automatically-enabled-checkers '(python-pylint))
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
  (setq sp-python-insert-colon-in-function-definitions nil)
  )
;;-- end smartparens

;;-- lsp

(defvar lsp-disabled-clients nil)
(add-to-list 'lsp-disabled-clients 'pyls)
(add-to-list 'lsp-disabled-clients 'pylsp)
(add-to-list 'lsp-disabled-clients 'mspyls)

(setq lsp-pyright-extra-paths #'[]
      lsp-pyright-venv-path   (list conda-env-home-directory)
      )
;;-- end lsp

;;-- jg-company
(defvar jg-python-company-activation (rx (| "error" "lib" "date" "argparse")))
(defvar jg-python-company-kws (let ((ht (make-hash-table :test 'equal)))
                                (puthash "error" jg-python-ivy-exceptions ht)
                                (puthash "lib" jg-python-ivy-libs ht)
                                (puthash "date" jg-python-ivy-datetimes ht)
                                (puthash "argparse" jg-python-ivy-argparse ht)
                                ht
                                )
  )
;;-- end jg-company

;;-- specs
(after! projectile
  (pushnew! projectile-project-root-files "pyproject.toml" "requirements.txt" "setup.py")
  (pushnew! projectile-project-root-files "setup.py" "requirements.txt")
  )
(spec-handling-add! projects
                   `(jg-python-project ("pyproject.toml") :project-file "pyproject.toml" :configure "pip install -e %s" :test "python -m unittest discover -v -p test_*.py" :test-dir (lambda (x) (f-join x "__tests")) :test-prefix "test_" :related-files-fn ,#'related-files:jg-python-project)
                   '(python-poetry ("poetry.lock") :project-file "poetry.lock" :compilation-dir nil :configure nil :compile "poetry build" :test "poetry run python -m unittest discover" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(python-pipenv ("Pipfile") :project-file "Pipfile" :compilation-dir nil :configure nil :compile "pipenv run build" :test "pipenv run test" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(python-tox ("tox.ini") :project-file "tox.ini" :compilation-dir nil :configure nil :compile "tox -r --notest" :test "tox" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(python-pkg ("setup.py") :project-file "setup.py" :compilation-dir nil :configure nil :compile "python setup.py build" :test "python -m unittest discover" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
                   '(python-pip ("requirements.txt") :project-file "requirements.txt" :compilation-dir nil :configure nil :compile "python setup.py build" :test "python -m unittest discover" :install nil :package nil :run nil :test-suffix "_test" :test-prefix "test_")
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
                     )
                    )
(spec-handling-add! file-templates
                    '(python
                      ("LICENSE\\'"        :trigger "__license-acab"   :mode text-mode :priority 100)
                      ("pyproject.toml\\'" :trigger "__pyproject"      :mode conf-toml-mode)
                      ("setup\\.cfg\\'"    :trigger "__setup_cfg"      :mode python-mode)
                      ("__init__\\.py\\'"  :trigger "__init"           :mode python-mode)
                      ("test_.+\\.py\\'"   :trigger "__pytest"         :mode python-mode)
                      ("cli_.+\\.py\\'"    :trigger "__cli"            :mode python-mode)
                      ("conf\\.py\\'"      :trigger "__conf"           :mode python-mode)
                      ("setup\\.py\\'"     :trigger "__setup"          :mode python-mode)
                      ("SConstruct"        :trigger "__sconstruct"     :mode python-mode)
                      ("SConscript"        :trigger "__sconscript"     :mode python-mode)
                      ("\\.py\\'"          :trigger "__"               :mode python-mode :priority -99)
                      (python-mode         :trigger "__" :priority -100)
                      )
                    )
(spec-handling-add! fold
                    `(python
                     :modes (python-mode)
                     :priority 25
                     :triggers (:close     ,#'+jg-python-close-class-defs
                                :close-all ,#'+jg-python-close-all-defs
                                :open      ,#'outline-toggle-children
                                :open-all  ,#'outline-show-all
                                :open-rec  ,#'outline-show-subtree
                                :toggle    ,#'outline-toggle-children
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
(spec-handling-add! company
                    '(python-mode (:front . jg-company/backend ) (:front . company-gtags))
                    '(anaconda-mode (:favour . company-anaconda))
                    )
(spec-handling-add! whitespace-cleanup
                    `(python-mode
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
(spec-handling-add! python-env
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
                      :definition ,#'+jg-conda-find-defs
                      :references ,#'+jg-conda-find-references
                      :documentation ,#'+jg-conda-show-doc)
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
                    '(python +jg-python-get-commands)
                    )
(set-repl-handler! 'python-mode #'+jg-python/open-repl
  :persist t
  :send-region #'python-shell-send-region
  :send-buffer #'python-shell-send-buffer
  )
(set-eval-handler! 'python-mode
    '((:command . (lambda () python-shell-interpreter))
      (:exec (lambda ()
               (if-let* ((bin (executable-find "pipenv" t))
                         (_ (pipenv-project-p)))
                   (format "PIPENV_MAX_DEPTH=9999 %s run %%c %%o %%s %%a" bin)
                 "%c %o %s %a")))
      (:description . "Run Python script"))
    )
;;-- end specs

(spec-handling-add! lookup-regular
                    '(python-mode
                      ("sh" . "https://amoffat.github.io/sh/")
                      ("% str formatting"  . "https://docs.python.org/3/library/stdtypes.html#printf-style-string-formatting")
                      ("{} str formatting" . "https://docs.python.org/3/library/string.html#formatspec")
                      ("Awesome Libs"      . "https://github.com/vinta/awesome-python")
                      ("datetime"          . "https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior")
                      ("PyGObject"         . "https://pygobject.readthedocs.io/en/latest/")
                      ("Pyright"           . "https://microsoft.github.io/pyright/#/configuration")
                      ("Logging"           . "https://docs.python.org/3/library/logging.html")
                      ("Match Statement"   . "https://docs.python.org/3/reference/compound_stmts.html#the-match-statement")
                      ("Match Spec PEP"    . "https://peps.python.org/pep-0634/")
                      ("Mocks"             . "https://docs.python.org/3/library/unittest.mock.html")
                      ("Unittest"          . "https://docs.python.org/3/library/unittest.html")
                      ("Twisted"           . "https://docs.twisted.org/en/stable/")
                      ("Mamba"             . "https://mamba.readthedocs.io/")
                      ("SnakeMake"         . "https://snakemake.readthedocs.io/")
                      ("Luigi"             . "https://luigi.readthedocs.io/en/stable/index.html")
                      ("Dagster"           . "https://docs.dagster.io/")
                      ("Petl"              . "https://petl.readthedocs.io/en/stable/index.html")
                      ("python-twitter"    . "https://python-twitter.readthedocs.io/en/latest/")
                      ("pyproject          .toml spec" . "https://packaging.python.org/en/latest/specifications/declaring-project-metadata/#declaring-project-metadata")
                      ("argparse"          . "https://docs.python.org/3/howto/argparse.html")
                      ("astroid"           . "https://pylint.pycqa.org/projects/astroid/en/latest/index.html")
                      ("beautiful Soup"    . "https://beautiful-soup-4.readthedocs.io/en/latest/")
                      ("bibtex Parser"     . "https://bibtexparser.readthedocs.io/en/master/tutorial.html")
                      ("black"             . "https://black.readthedocs.io/en/stable/")
                      ("boltons"           . "https://boltons.readthedocs.io/en/latest/")
                      ("build"             . "https://github.com/pypa/build")
                      ("cairo"             . "https://pycairo.readthedocs.io/en/latest/reference/context.html")
                      ("cleo"              . "https://cleo.readthedocs.io/en/latest/")
                      ("conda"             . "https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html")
                      ("construct"         . "https://construct.readthedocs.io/en/latest/")
                      ("dirty-equals"      . "https://dirty-equals.helpmanual.io/latest/")
                      ("doit"              . "https://pydoit.org/contents.html")
                      ("floweaver"         . "https://floweaver.readthedocs.io/en/latest/")
                      ("functools"         . "https://docs.python.org/3/library/functools.html")
                      ("graphviz"          . "https://graphviz.readthedocs.io/en/stable/")
                      ("imageio"           . "https://imageio.readthedocs.io/en/stable/index.html")
                      ("itertools"         . "https://docs.python.org/3/library/itertools.html")
                      ("jinja"             . "https://palletsprojects.com/p/jinja/")
                      ("llvmlite"          . "https://llvmlite.readthedocs.io/en/latest/index.html")
                      ("marshmallow"       . "https://marshmallow.readthedocs.io/en/stable/")
                      ("matplotlib"        . "https://matplotlib.org/stable/api/index.html")
                      ("more-itertools"    . "https://more-itertools.readthedocs.io/en/stable/")
                      ("networkx"          . "https://networkx.github.io/")
                      ("numpy"             . "https://numpy.org/doc/stable/")
                      ("pandas"            . "https://pandas.pydata.org/docs/")
                      ("pathlib"           . "https://docs.python.org/3/library/pathlib.html")
                      ("pelican"           . "https://docs.getpelican.com/en/latest/")
                      ("pgmpy"             . "https://pgmpy.org/")
                      ("pillow"            . "https://pillow.readthedocs.io/en/stable/")
                      ("pip"               . "https://pip.pypa.io/en/stable/")
                      ("pipx"              . "https://pypa.github.io/pipx/")
                      ("poetry"            . "https://python-poetry.org/")
                      ("pomegranate"       . "https://pomegranate.readthedocs.io/en/latest/")
                      ("pony"              . "https://docs.ponyorm.org/")
                      ("pre-commit"        . "https://pre-commit.com/")
                      ("pronouncingpy"     . "https://pronouncing.readthedocs.io/en/latest/")
                      ("pyRight"           . "https://github.com/Microsoft/pyright")
                      ("pycycle"           . "https://github.com/bndr/pycycle")
                      ("pyexcel"           . "https://github.com/pyexcel/pyexcel")
                      ("pygments"          . "https://pygments.org/docs/")
                      ("pylightxl"         . "https://github.com/PydPiper/pylightxl")
                      ("pyparsing"         . "https://pyparsing-docs.readthedocs.io/en/latest/")
                      ("pypi"              . "https://pypi.org/")
                      ("python stdlib"     . "https://docs.python.org/3/library/index.html")
                      ("python/C api"      . "https://docs.python.org/3/c-api/index.html")
                      ("railroad-diagrams" . "https://github.com/tabatkins/railroad-diagrams")
                      ("re"                . "https://docs.python.org/3/library/re.html")
                      ("rich"              . "https://rich.readthedocs.io/en/stable/introduction.html")
                      ("scikit-learn"      . "https://scikit-learn.org/stable/user_guide.html")
                      ("scrapy"            . "https://docs.scrapy.org/en/latest/")
                      ("scons"             . "https://scons.org/documentation.html")
                      ("seaborn"           . "https://seaborn.pydata.org/api.html")
                      ("setuptools"        . "https://setuptools.readthedocs.io/en/latest/userguide/index.html")
                      ("setuptools-rust"   . "https://setuptools-rust.readthedocs.io/en/latest/")
                      ("sorobn"            . "https://github.com/MaxHalford/sorobn")
                      ("spacy"             . "https://spacy.io/usage")
                      ("sphinx"            . "https://www.sphinx-doc.org/en/master/contents.html")
                      ("stackprinter"      . "https://github.com/cknd/stackprinter")
                      ("string"            . "https://docs.python.org/3/library/string.html")
                      ("sty"               . "https://github.com/feluxe/sty")
                      ("sympy"             . "https://docs.sympy.org/latest/guides/index.html")
                      ("thefuzz"           . "https://github.com/seatgeek/thefuzz")
                      ("tomllib"           . "https://docs.python.org/3/library/tomllib.html")
                      ("toolz"             . "https://toolz.readthedocs.io/en/latest/")
                      ("tqdm"              . "https://tqdm.github.io/")
                      ("validators"        . "https://python-validators.github.io/validators/")
                      ("venv"              . "https://docs.python.org/3/library/venv.html")
                      ("xsdata"            . "https://xsdata.readthedocs.io/en/latest/")
                      ("xsdata-plantuml"   . "https://github.com/tefra/xsdata-plantuml")
                      ("z3"                . "https://github.com/Z3Prover/z3")
                      ("zlib"              . "https://docs.python.org/3/library/zlib.html")
                      ("Martin Heinz"      . "https://martinheinz.dev/")
                      ("Pytest" . "https://docs.pytest.org/en/7.3.x/how-to/fixtures.html")
                      ("Pytest Mock" . "https://pytest-mock.readthedocs.io/en/latest/")
                     )
                    '(conf-mode
                     ("setuptools-rust"   . "https://setuptools-rust.readthedocs.io/en/latest/")
                     ("pypi"              . "https://pypi.org/")
                     ("setuptools toml" . "https://setuptools.pypa.io/en/latest/userguide/pyproject_config.html")
                     ("pip toml" . "https://pip.pypa.io/en/stable/reference/build-system/pyproject-toml/")
                     )
                    '(manifest-mode
                     ("Manifest format" . "https://docs.python.org/3/distutils/sourcedist.html?highlight=manifest")
                     )
                    )
