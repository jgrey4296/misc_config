;;; lang/jg-python/+bindings.el -*- lexical-binding: t; -*-

(map! :after python
      :map python-mode-map
      :n "z d" nil ;; #'+jg-python-toggle-all-defs
      :n "z D" nil ;; #'+jg-python-close-class-defs
      :v "i f" #'+jg-python-select-defun
      :v "i F" #'+jg-python-select-class
      :leader
      (:prefix ("i" . "Insert")
        :desc "Insert Breakpoint" "d" #'+jg-python-toggle-breakpoint
       )
      :localleader
      :desc "Sort defs" "S" #'+jg-python-sort-class-methods
      :desc "REPL"      "r" #'+python/open-repl
      )

(map! :after python
      :map python-mode-map
      :localleader
      (:prefix ("j" . "Jump")
       :desc "Docs: Python"        "1" (cmd! (+jg-browse-url jg-python-docs-url))
       :desc "Docs: Bibtex Parser" "2" (cmd! (+jg-browse-url jg-python-bibtex-parser-url))
       :desc "Docs: BeautifulSoup" "3" (cmd! (+jg-browse-url jg-python-beautiful-soup-url))
       )
      (:prefix ("i" . "imports")
       :desc "Insert missing imports" "i" #'pyimport-insert-missing
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
       :desc "activate"    "a" #'pyvenv-activate
       :desc "deactivate"  "d" #'pyvenv-deactivate)
      )

(map! :after cython-mode
      :map cython-mode-map
      :localleader
      (:prefix "c"
      :desc "Cython compile buffer"    "c" #'cython-compile))

(defun +jg-python-conda-override ()
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
