;;; lang/jg-python/+bindings.el -*- lexical-binding: t; -*-

(map! :map dired-mode-map
      :localleader
      :n "v" #'+jg-python-activate-venv-and-conda
      )

(map! :after python
      :map python-mode-map
      :n "z d" nil ;; #'+jg-python-toggle-all-defs
      :n "z D" nil ;; #'+jg-python-close-class-defs
      :v "i f" #'+jg-python-select-defun
      :v "i F" #'+jg-python-select-class
      :localleader
      :desc "Sort defs" "S" #'+jg-python-sort-class-methods
      :desc "REPL"      "r" #'+python/open-repl
      :desc "debug"     "d" (cmd! (setq jg-python-dev-mode (not jg-python-dev-mode))
                                  (message "Python Debug Mode: %s" jg-python-dev-mode))
      )

(map! :after (python pyimport)
      :map python-mode-map
      :localleader
      (:prefix ("i" . "imports")
       :desc "Insert import"          "i" #'+jg-python-insert-import
       :desc "Insert Import Snippet"  "I" #'+jg-python-import-snippet
       :desc "Remove unused imports"  "r" #'pyimport-remove-unused
       :desc "Optimize imports"       "o" #'+python/optimize-imports
       :desc "Sort imports"           "s" #'py-isort-buffer
       :desc "Sort region"            "r" #'py-isort-region)
      (:prefix ("t" . "test")
       :desc "Test DWIM"       "f" #'python-pytest-file-dwim
       :desc "Test File"       "F" #'python-pytest-file
       :desc "Test Func DWIM"  "t" #'python-pytest-function-dwim
       :desc "Test Func"       "T" #'python-pytest-function
       :desc "Test repeat"     "r" #'python-pytest-repeat
       :desc "Test Popup"      "p" #'python-pytest-popup)
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
      :desc "Docs: Python"        "1" (cmd! (+jg-browse-url jg-python-docs-url))
      :desc "Docs: Bibtex Parser" "2" (cmd! (+jg-browse-url jg-python-bibtex-parser-url))
      :desc "Docs: BeautifulSoup" "3" (cmd! (+jg-browse-url jg-python-beautiful-soup-url))
      :desc "Docs: FlowWeaver"    "4" (cmd! (+jg-browse-url "https://floweaver.readthedocs.io/en/latest/"))
      :desc "Docs: Matplotlib"    "5" (cmd! (+jg-browse-url "https://matplotlib.org/stable/contents.html"))
      :desc "Docs: Numpy"         "6" (cmd! (+jg-browse-url "https://numpy.org/doc/stable/"))
      :desc "Docs: Pandas"        "7" (cmd! (+jg-browse-url "https://pandas.pydata.org/docs/"))
      :desc "Docs: Seaborn"       "8" (cmd! (+jg-browse-url "https://seaborn.pydata.org/api.html"))
      :desc "Docs: PyPi"          "9" (cmd! (+jg-browse-url "https://pypi.org/"))
      :desc "Docs: Setuptools"    "0" (cmd! (+jg-browse-url "https://setuptools.readthedocs.io/en/latest/userguide/index.html"))
      :desc "Docs: Conda Env"     "E" (cmd! (+jg-browse-url "https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html"))
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
        :desc "Conda: Find Defs"    "d" #'anaconda-mode-find-definitions
        :desc "Conda: Show Doc"     "h" #'anaconda-mode-show-doc
        :desc "Conda: Find Assigns" "a" #'anaconda-mode-find-assignments
        :desc "Conda: Find File"    "f" #'anaconda-mode-find-file
        :desc "Conda: Find Refs"    "u" #'anaconda-mode-find-references
        )
  )

(map! :after conf-mode
      :map conf-mode-map
      :localleader
      :desc "Docs: Setuptools"     "1" (cmd! (+jg-browse-url "https://setuptools.pypa.io/en/latest/index.html"))

      )

(map! :map manifest-mode-map
      :localleader
      :desc "Docs: Manifest files"  "1" (cmd! (+jg-browse-url "https://docs.python.org/3/distutils/sourcedist.html?highlight=manifest"))
      )
