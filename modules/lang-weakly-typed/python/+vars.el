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
                    '(python-mode (:front . jg-company/backend ) (:front . company-gtags))
                    '(anaconda-mode (:favour . company-anaconda))
                    )
(spec-handling-add! whitespace-cleanup nil
                    '(python-mode
                      +jg-python-cleanup-ensure-newline-before-def
                      delete-trailing-whitespace
                      +jg-text-cleanup-whitespace
                     )
                    )
(spec-handling-add! modeline nil
                    '(python
                      (env-handling-state (:eval (env-handling-state-line)))
                      )
                    )
(spec-handling-add! python-env t
                    '(default (:setup . none) (:support  . none))
                    '(pyright (:support . lsp))
                    )
(spec-handling-add! ligatures t
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

(spec-handling-add! lookup-url nil
                    '(python
                     ("Python" "https://docs.python.org/3/search.html?q=%s&check_keywords=yes&area=default")
                     ("Pypi"   "https://pypi.org/search/?q=%s")
                     )
                    )
(spec-handling-add! lookup-regular nil
                    '(python-mode
                      ("pyproject.toml spec" . "https://packaging.python.org/en/latest/specifications/declaring-project-metadata/#declaring-project-metadata")
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
                      ("pypi"              . "https://pypi .org/")
                      ("python"            . "https://docs.python.org/3/")
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
                     )
                    '(conf-mode
                     ("Setuptools" . "https://setuptools.readthedocs.io/en/latest/userguide/index.html")
                     )
                    '(manifest-mode
                     ("Manifest format" . "https://docs.python.org/3/distutils/sourcedist.html?highlight=manifest")
                     )
                    )
(spec-handling-add! lookup-handler nil
                    '(anaconda-mode
                      :definition +jg-conda-find-defs
                      :references +jg-conda-find-references
                      :documentation +jg-conda-show-doc)
                    )
(spec-handling-add! docsets
                    '((python-mode inferior-python-mode)
                      "Python 3" "NumPy" "SciPy" "Pandas"
                      )
                    )
(set-repl-handler! 'python-mode #'+jg-python/open-repl
  :persist t
  :send-region #'python-shell-send-region
  :send-buffer #'python-shell-send-buffer)
(set-eval-handler! 'python-mode
    '((:command . (lambda () python-shell-interpreter))
      (:exec (lambda ()
               (if-let* ((bin (executable-find "pipenv" t))
                         (_ (pipenv-project-p)))
                   (format "PIPENV_MAX_DEPTH=9999 %s run %%c %%o %%s %%a" bin)
                 "%c %o %s %a")))
      (:description . "Run Python script")))
;;-- end specs
