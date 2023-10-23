;;; lang/jg-python/+vars.el -*- lexical-binding: t; -*-

;;-- general python

(setq expand-region-preferred-python-mode 'python-mode)

(defvaralias 'python-indent-offset 'py-indent-offset)

(defvaralias 'python-pdbtrack-activate 'py-pdbtrack-do-tracking-p)

(defvaralias 'python-shell--interpreter 'py-python-command)

(defvaralias 'python-shell-virtualenv-root 'py-shell-virtualenv-root)

(setq py-indent-offset 4
      conda-anaconda-home (or (getenv "MAMBA_ROOT_PREFIX") (getenv "ANACONDA_HOME") "/usr/local/anaconda3")
      conda-env-home-directory (cond ((getenv "MAMBA_ROOT_PREFIX")
                                      (f-join (getenv "MAMBA_ROOT_PREFIX") "envs"))
                                     ((getenv "ANACONDA_ENVS")
                                      (getenv "ANACONDA_ENVS"))
                                     (t
                                      (f-join conda-anaconda-home "envs"))
                                     )
      py-shell-virtualenv-root conda-env-home-directory

      py-pdbtrack-do-tracking-p t

      py-python-command "python3"
      py-python-command-args '("-i")
      python-shell--interpreter-args "-i"

      py-use-font-lock-doc-face-p t
      py-fontify-shell-buffer-p t

      python-indent-guess-indent-offset nil
      python-shell-completion-native-enable nil
      python-shell-completion-native-disabled-interpreters '("pypy")

      jg-python-repl-start-file (expand-file-name "~/.config/jg/templates/python/repl_startup.py ")
      python-shell-interpreter-path-args (expand-file-name "~/.config/jg/templates/python/repl_startup.py ")
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
(setq lsp-pyright-extra-paths #'[]
      lsp-pyright-venv-path conda-env-home-directory
      lsp-pyright-log-level "Information" ;; Error Warning Information Trace
      lsp-pyright-typechecking-mode "basic"

      lsp-ruff-lsp-advertize-fix-all            nil
      lsp-ruff-lsp-advertize-organize-imports   t
      lsp-ruff-lsp-show-notifications           "always"
      lsp-ruff-lsp-log-level                    "info"
      lsp-ruff-lsp-server-command               '("ruff-lsp")
      lsp-ruff-lsp-python-path                  "python3"
      lsp-ruff-lsp-import-strategy              "fromEnvironment"
      lsp-ruff-lsp-ruff-args                    [ ]
      lsp-ruff-lsp-ruff-path                    ["ruff"]

      lsp-pylsp-configuration-sources []
      lsp-pylsp-server-command '("pylsp")
      lsp-pylsp-plugins-flake8-enabled nil

      )

;;-- end lsp

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
                     )
                    )
(spec-handling-add! file-templates
                    '(python
                      ("LICENSE\\'"            :trigger "__license-acab"   :mode text-mode   :priority 100)
                      ;;Configs:
                      ("pyproject.toml\\'"     :trigger "__pyproject"      :mode python-mode)
                      ("pyrightconfig.json\\'" :trigger "__pyright_config" :mode python-mode)
                      ("pylint.toml"           :trigger "__pylint"         :mode python-mode)
                      ("jekyl.toml"            :trigger "__jekyll"         :mode python-mode)
                      ("ruff.toml"  trigger "__ruff_config"                :mode python-mode)

                      ;; Python:
                      ("__init__\\.py\\'"      :trigger "__init"           :mode python-mode)
                      ("conf.py"               :trigger "__sphinx_conf"    :mode python-mode)
                      ("log_config.py"         :trigger "__log_config"     :mode python-mode)
                      ("test_.+\\.py\\'"       :trigger "__pytest"         :mode python-mode)
                      ("cli_.+\\.py\\'"        :trigger "__cli"            :mode python-mode)
                      ("\\.py\\'"              :trigger "__"               :mode python-mode :priority -99)
                      (python-mode             :trigger "__"                                 :priority -100)
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
                    '(python +jg-python-get-commands +jg-python-solo-file-run)
                    )
(spec-handling-add! eval :form 'override
                    `(python-mode
                      :start ,#'+jg-python/open-repl
                      :send ,#'python-shell-send-region
                      :eval nil
                      )
                    `(ipython
                      :start ,#'+jg-python/open-ipython-repl
                      :send ,#'python-shell-send-region
                      :eval nil
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
;;-- end specs

;;-- general insert
(general-insert-register-processor 'python-mode "raise"
                                   #'(lambda (x) (insert "raise " (s-replace-regexp "^[^A-Z]+" "" x))))
(general-insert-register-processor 'python-mode "datetime"
                                   #'(lambda (x) (insert (car (split-string x " " t " +")))))
(general-insert-register-processor 'python-mode "fixtures"
                                   #'(lambda (x) (insert (car (split-string x " " t " "+)))))
(general-insert-register-processor 'python-mode "import"
                                   #'(lambda (x) (insert "import " (car (split-string x t " +")))))

;;-- end general insert
