;;; lang/jg-python/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :prefix ("c v" . "Environments")
      :desc "Activate Env" "a" #'env-handling-go!
      :desc "Clear Env"    "d" #'env-handling-clear-env!
      :desc "Report Env"   "r" #'env-handling-report!
      :desc "Lock Env"     "l" #'env-handling-lock!
      :desc "Create venv"  "c" #'env-handling-create-env!

      )

(map! :map python-mode-map
      :after python-mode
      :desc "General Insert"         :n "|" #'general-insert-call
      :n "z d" nil ;; #'+jg-python-toggle-all-defs
      :n "z D" nil ;; #'+jg-python-close-class-defs
      :v "i f" nil ;; #'+jg-python-select-defun
      :v "i F" nil ;; #'+jg-python-select-class
      ;; :n "] ]" #'+jg-python-forward-defun
      ;; :n "s j" '+jg-python-swipe-to-def
      :n "SPC c t" #'+jg-python-pytest-dispatch
      :n "SPC \\"   #'+jg-python-popup-related-test
      )

(map! :map python-mode-map ;; localleader
      :after python-mode
      :localleader
      :desc "Ruff Format" "f" #'+jg-python-ruff-format
      :desc "Start Pydoc" "p" #'+jg-python-start-pydoc
      :desc "Summarize"   "\\" #'+jg-python-summarize
      :desc "REPL"        "r" #'+jg-python/open-ipython-repl
      :desc "debug"       "d" (cmd! (setq jg-python-dev-mode (not jg-python-dev-mode))
                                  (message "Python Debug Mode: %s" jg-python-dev-mode))
      :desc "track"       ";" #'py-pdbtrack-toggle-stack-tracking
      :desc "breakpoint"  "b" #'+jg-python-breakpoint-line
      :desc "Disassemble" "D" #'+jg-python-bytecode-dwim
      )

(map! :map python-mode-map ;; localleader.imports
      :localleader
      :desc "Import View"            "i" #'+jg-python--import-view
      :desc "Sort imports"           "s" #'+jg-python-isort-diff
      :prefix ("I" . "imports")
      :desc "Insert Import Snippet"  "I" #'+jg-python-import-snippet
       )

(map! :map python-mode-map ;; localleader.tests
      :localleader
      :prefix ("t" . "test")
      :desc "Test DWIM"       "f" #'python-pytest-file-dwim
      :desc "Test File"       "F" #'python-pytest-file
      :desc "Test Func DWIM"  "t" #'python-pytest-function-dwim
      :desc "Test Func"       "T" #'python-pytest-function
      :desc "Test repeat"     "r" #'python-pytest-repeat
      :desc "Test Popup"      "P" #'python-pytest-dispatch
      :desc "Test Popup"      "p" #'+jg-python-pytest-dispatch
      :desc "Make Testfile"   "m" #'+jg-python-make-test-file
      )

(map! :map python-mode-map ;; localleader.environment
      :localleader
      :prefix ("e" . "Environment")
      :desc "Choose Support" "c" #'+jg-python-support
      :desc "Current Support" "C" (cmd! (message "Current Python Support: %s" jg-python-last-chosen-support))
      :desc "install"     "i" #'pipenv-install
      :desc "pipenv lock" "l" #'pipenv-lock
      :desc "open module" "o" #'pipenv-open
      :desc "run"         "r" #'pipenv-run
      :desc "shell"       "s" #'pipenv-shell
      :desc "uninstall"   "u" #'pipenv-uninstall
      )

(map! :map inferior-python-mode-map
      :after python-mode
      "TAB" #'+jg-snippets-complete-or-snippet
      )

(map! :map cython-mode-map
      :after cython-mode
      :localleader
      (:prefix "c"
      :desc "Cython compile buffer"    "c" #'cython-compile))

(map! :map anaconda-mode-map
      :after anaconda-mode
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

(map! :map doot-mode-map
      :n "s <" #'doot-open-toml
      )

(map! :map jg-dired-mode-map
      :desc "Make Test dir"  :n "< t" (cmd! (dired-create-directory "__tests"))
      :desc "Make Docs dir"  :n "< d" (cmd! (dired-create-directory "docs"))
      :desc "Disassemble Python" :n "d ? p" #'+jg-python-dired-dis
      :localleader
      :desc "Try Python site-packages" "p" #'+jg-python-try-site-packages

      )

(map! :map comint-mode-map
      :localleader
      :desc "PdbTrack" ";" #'py-pdbtrack-toggle-stack-tracking
      )

(map! :map py-test-minor-mode-map
      :n "RET" #'py-test-minor-function-dwim
      :i "RET" #'evil-ret
      :n "DEL" #'py-test-copy-current-test
      )
