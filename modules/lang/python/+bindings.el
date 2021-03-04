;;; lang/python/+bindings.el -*- lexical-binding: t; -*-

(map! :after python
      :map python-mode-map
      :n "z d" '+jg-personal-toggle-all-defs
      :n "z D" '+jg-personal-close-class-defs
      :leader
      (:prefix ("i" . "Insert")
        :desc "Insert Breakpoint" "d" #'+jg-personal-python-toggle-breakpoint
       )
      (:prefix ("j" . "Jump")
        :desc "Create Tags" "C" 'helm-gtags-create-tags
        :desc "Find Tag" "d" 'helm-gtags-find-tag
        :desc "Fing Tag Other Window" "D" 'helm-gtags-find-tag-other-window
        :desc "DWIM Other Window" "G" 'helm-gtags-dwim-other-window
        :desc "Tags in func" "i" 'helm-gtags-tags-in-this-function
        :desc "Parse File" "l" 'helm-gtags-parse-file
        :desc "History: Next" "n" 'helm-gtags-next-history
        :desc "History: Prev" "p" 'helm-gtags-previous-history
        :desc "Find rtag" "r" 'helm-gtags-find-rtag
        :desc "Gtags Resume" "R" 'helm-gtags-resume
        :desc "Gtags Select" "s" 'helm-gtags-select
        :desc "Gtags show stack" "S" 'helm-gtags-show-stack
        :desc "Find Symbol" "y" 'helm-gtags-find-symbol
        :desc "Update Tags" "U" 'helm-gtags-update-tags
       )
      ;; -----------
      :localleader
      (:prefix ("i" . "imports")
       :desc "Insert missing imports" "i" #'pyimport-insert-missing
       :desc "Remove unused imports"  "r" #'pyimport-remove-unused
       :desc "Optimize imports"       "o" #'+python/optimize-imports
       :desc "Sort imports"           "s" #'py-isort-buffer
       :desc "Sort region"            "r" #'py-isort-region)
      (:prefix ("t" . "test")
       :desc "Test DWIM" "f" #'python-pytest-file-dwim
       :desc "Test File" "F" #'python-pytest-file
       :desc "Test Func DWIM" "t" #'python-pytest-function-dwim
       :desc "Test Func" "T" #'python-pytest-function
       :desc "Test repeat" "r" #'python-pytest-repeat
       :desc "Test Popup" "p" #'python-pytest-popup)
      (:prefix ("e" . "Environment")
       :desc "activate"    "a" #'pipenv-activate
       :desc "deactivate"  "d" #'pipenv-deactivate
       :desc "install"     "i" #'pipenv-install
       :desc "lock"        "l" #'pipenv-lock
       :desc "open module" "o" #'pipenv-open
       :desc "run"         "r" #'pipenv-run
       :desc "shell"       "s" #'pipenv-shell
       :desc "uninstall"   "u" #'pipenv-uninstall))

;; NOTE: normal macro expansion adds :major-modes t, which doesn't work for minor modes
;; (map! :after anaconda-mode
;;       :map anaconda-mode-map
;;       :localleader
;;       (:prefix ("g" . "Goto")
;;        :desc "Find Definitions" "d" #'anaconda-mode-find-definitions
;;        :desc "Show Docs" "h"        #'anaconda-mode-show-doc
;;        :desc "Find Assignments" "a" #'anaconda-mode-find-assignments
;;        :desc "Find File" "f"        #'anaconda-mode-find-file
;;        :desc "Find References" "u"  #'anaconda-mode-find-references))

(after! anaconda-mode
  (general-define-key :states
                    '(normal visual motion emacs insert)
                    :prefix doom-localleader-key :non-normal-prefix doom-localleader-alt-key
                    :keymaps '(anaconda-mode-map)
                    :infix "g" "" (list :ignore t :which-key "Goto")
                    "d" (list :def #'anaconda-mode-find-definitions :which-key "Find Definitions")
                    "h" (list :def #'anaconda-mode-show-doc :which-key "Show Docs")
                    "a" (list :def #'anaconda-mode-find-assignments :which-key "Find Assignments")
                    "f" (list :def #'anaconda-mode-find-file :which-key "Find File")
                    "u" (list :def #'anaconda-mode-find-references :which-key "Find References"))
  )

(map! :localleader
      :after nose
      :map nose-mode-map
      (:prefix ("t" . "Test")
       "r" #'nosetests-again
       "a" #'nosetests-all
       "s" #'nosetests-one
       "v" #'nosetests-module
       "A" #'nosetests-pdb-all
       "O" #'nosetests-pdb-one
       "V" #'nosetests-pdb-module))

(map! :after cython-mode
      :map cython-mode-map
      :localleader
      (:prefix "c"
      :desc "Cython compile buffer"    "c" #'cython-compile))
