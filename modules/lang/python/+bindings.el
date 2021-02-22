;;; lang/python/+bindings.el -*- lexical-binding: t; -*-

(map! :after python
      :map python-mode-map
      :n "z d" '+jg-personal-toggle-all-defs
      :n "z D" '+jg-personal-close-class-defs
      :leader
      (:prefix ("i" . "Insert")
       :n "d" #'+jg-personal-python-toggle-breakpoint
       )
      (:prefix ("j" . "Jump")
       :n "C" 'helm-gtags-create-tags
       :n "d" 'helm-gtags-find-tag
       :n "D" 'helm-gtags-find-tag-other-window
       :n "G" 'helm-gtags-dwim-other-window
       :n "i" 'helm-gtags-tags-in-this-function
       :n "l" 'helm-gtags-parse-file
       :n "n" 'helm-gtags-next-history
       :n "p" 'helm-gtags-previous-history
       :n "r" 'helm-gtags-find-rtag
       :n "R" 'helm-gtags-resume
       :n "s" 'helm-gtags-select
       :n "S" 'helm-gtags-show-stack
       :n "y" 'helm-gtags-find-symbol
       :n "U" 'helm-gtags-update-tags
       ))

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

(map! :after python
      :map python-mode-map
      :localleader
      (:prefix ("i" . "imports")
       :desc "Insert missing imports" "i" #'pyimport-insert-missing
       :desc "Remove unused imports"  "r" #'pyimport-remove-unused
       :desc "Optimize imports"       "o" #'+python/optimize-imports
       :desc "Sort imports"           "s" #'py-isort-buffer
       :desc "Sort region"            "r" #'py-isort-region))

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

(map! :after python
      :localleader
      :map python-mode-map
      (:prefix ("t" . "test")
      "f" #'python-pytest-file-dwim
      "F" #'python-pytest-file
      "t" #'python-pytest-function-dwim
      "T" #'python-pytest-function
      "r" #'python-pytest-repeat
      "p" #'python-pytest-popup))

(map! :map python-mode-map
      :localleader
      (:prefix ("e" . "Environment")
      :desc "activate"    "a" #'pipenv-activate
      :desc "deactivate"  "d" #'pipenv-deactivate
      :desc "install"     "i" #'pipenv-install
      :desc "lock"        "l" #'pipenv-lock
      :desc "open module" "o" #'pipenv-open
      :desc "run"         "r" #'pipenv-run
      :desc "shell"       "s" #'pipenv-shell
      :desc "uninstall"   "u" #'pipenv-uninstall))

(map! :after cython-mode
      :map cython-mode-map
      :localleader
      (:prefix "c"
      :desc "Cython compile buffer"    "c" #'cython-compile))
