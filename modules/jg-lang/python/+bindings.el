;;; lang/jg-python/+bindings.el -*- lexical-binding: t; -*-

(map! :after dired
      :map dired-mode-map
      :localleader
      :desc "Activate Py Environments" :n "v" #'+jg-python-activate-venv-and-conda
      )

(map! :after python
      :map python-mode-map
      :n "z d" nil ;; #'+jg-python-toggle-all-defs
      :n "z D" nil ;; #'+jg-python-close-class-defs
      :v "i f" #'+jg-python-select-defun
      :v "i F" #'+jg-python-select-class
      :n "] ]" #'+jg-python-forward-defun
      :localleader
      ;; :desc "Sort defs" "S" #'+jg-python-sort-class-methods
      :desc "Summarize" "s" #'+jg-python-summarize
      :desc "REPL"      "r" #'+python/open-repl
      :desc "debug"     "d" (cmd! (setq jg-python-dev-mode (not jg-python-dev-mode))
                                  (message "Python Debug Mode: %s" jg-python-dev-mode))
      )

(map! :after (python)
      :map python-mode-map
      :localleader
      (:prefix ("i" . "imports")
       :desc "Insert import"          "i" #'+jg-python-insert-import
       :desc "Insert Import Snippet"  "I" #'+jg-python-import-snippet
       :desc "Sort imports"           "s" #'py-isort-buffer
       :desc "Sort region"            "r" #'py-isort-region
       :desc "Collect import blocks"  "c" #'+jg-python-cleanup-import-blocks)
      (:prefix ("t" . "test")
       :desc "Test DWIM"       "f" #'python-pytest-file-dwim
       :desc "Test File"       "F" #'python-pytest-file
       :desc "Test Func DWIM"  "t" #'python-pytest-function-dwim
       :desc "Test Func"       "T" #'python-pytest-function
       :desc "Test repeat"     "r" #'python-pytest-repeat
       :desc "Test Popup"      "p" #'python-pytest-dispatch)
      (:prefix ("e" . "Environment")
       :desc "activate"    "a"    #'pyvenv-activate
       :desc "deactivate"  "d"    #'pyvenv-deactivate
       :desc "Choose Support" "c" #'+jg-python-support
       :desc "Current Support" "C" (cmd! (message "Current Python Support: %s" jg-python-last-chosen-support))
       )
      )

(map! :after python
      :map (python-mode-map inferior-python-mode-map)
      :localleader
      :desc "Docs: Python"        "1" (cmd! (+jg-misc-browse-url (s-concat jg-python-docs-url (let ((lib (read-string "Library: ")))
                                                                                           (if (s-blank? lib) nil (format jg-python-lib-url-suffix lib))))))
      :desc "Docs: PyPi"          "2" (cmd! (+jg-misc-browse-url (s-concat "https://pypi.org/" (let ((lib (read-string "Library: ")))
                                                                                            (if (s-blank? lib) nil (format "search/?q=%s" lib))))))
      (:prefix ("3" . "Lib Docs")
       :desc "Docs: Bibtex Parser" "1" (cmd! (+jg-misc-browse-url jg-python-bibtex-parser-url))
       :desc "Docs: BeautifulSoup" "2" (cmd! (+jg-misc-browse-url jg-python-beautiful-soup-url))
       :desc "Docs: PyRight"       "3" (cmd! (+jg-misc-browse-url "https://github.com/Microsoft/pyright"))
       :desc "Docs: Z3"            "4" (cmd! (+jg-misc-browse-url "https://github.com/Z3Prover/z3"))
       :desc "Docs: Sorobn"        "5" (cmd! (+jg-misc-browse-url "https://github.com/MaxHalford/sorobn"))
       :desc "Docs: Pomegranate"   "6" (cmd! (+jg-misc-browse-url "https://pomegranate.readthedocs.io/en/latest/"))
       :desc "Docs: Pgmpy"         "7" (cmd! (+jg-misc-browse-url "https://pgmpy.org/"))
       :desc "Docs: Rich"          "0" (cmd! (+jg-misc-browse-url "https://rich.readthedocs.io/en/stable/introduction.html"))
       )
      (:prefix ("4" . "Data Science Docs")
       :desc "Docs: FlowWeaver"    "1" (cmd! (+jg-misc-browse-url "https://floweaver.readthedocs.io/en/latest/"))
       :desc "Docs: Matplotlib"    "2" (cmd! (+jg-misc-browse-url "https://matplotlib.org/stable/contents.html"))
       :desc "Docs: Numpy"         "3" (cmd! (+jg-misc-browse-url "https://numpy.org/doc/stable/"))
       :desc "Docs: Pandas"        "4" (cmd! (+jg-misc-browse-url "https://pandas.pydata.org/docs/"))
       :desc "Docs: Seaborn"       "5" (cmd! (+jg-misc-browse-url "https://seaborn.pydata.org/api.html"))
       )
      (:prefix ("5" . "Database")
       :desc "Docs: Pony"           "1" (cmd! (+jg-misc-browse-url "https://github.com/ponyorm/pony/"))
       :desc "Docs: Marshmallow"    "2" (cmd! (+jg-misc-browse-url "https://marshmallow.readthedocs.io/en/stable/"))
       :desc "Docs: pylightxl"      "3" (cmd! (+jg-misc-browse-url "https://github.com/PydPiper/pylightxl"))
       :desc "Docs: pyexcel"        "4" (cmd! (+jg-misc-browse-url "https://github.com/pyexcel/pyexcel"))
      )
      (:prefix ("0" . "Packaging Docs")
       :desc "Docs: Setuptools"    "1" (cmd! (+jg-misc-browse-url "https://setuptools.readthedocs.io/en/latest/userguide/index.html"))
       :desc "Docs: Conda Env"     "2" (cmd! (+jg-misc-browse-url "https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html"))
      )
)

(map! :after cython-mode
      :map cython-mode-map
      :localleader
      (:prefix "c"
      :desc "Cython compile buffer"    "c" #'cython-compile))

(defun +jg-python-conda-binding-override ()
  (map! :map anaconda-mode-map
        :localleader
        "g" nil
        :prefix ("j" . "Jump")
        ;; :desc "Conda: Find Defs"    "d" #'anaconda-mode-find-definitions
        :desc "Conda: Find Defs"    "d" #'+jg-conda-find-defs
        :desc "Conda: Show Doc"     "h" #'+jg-conda-show-doc
        :desc "Conda: Find Assigns" "a" #'+jg-conda-find-assignments
        :desc "Conda: Find Refs"    "u" #'+jg-conda-find-references
        :desc "Conda: Eldoc"        "e" #'+jg-conda-eldoc
        )
  )

(map! :after conf-mode
      :map conf-mode-map
      :localleader
      :desc "Docs: Setuptools"     "1" (cmd! (+jg-misc-browse-url "https://setuptools.pypa.io/en/latest/index.html"))

      )

(map! :map manifest-mode-map
      :localleader
      :desc "Docs: Manifest files"  "1" (cmd! (+jg-misc-browse-url "https://docs.python.org/3/distutils/sourcedist.html?highlight=manifest"))
      )
