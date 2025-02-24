;;; lang/jg-python/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Select Python Repl" "c r p" #'+jg-python-select-repl
      )

(map! :map (python-mode-map python-ts-mode-map)
      :after python-mode
      :desc "General Insert"         :n "|" #'librarian-insert-trigger
      :n "z d" nil
      :n "z D" nil
      :v "i f" nil
      :v "i F" nil
      ;; :n "] ]" #'+jg-python-forward-defun
      ;; :n "s j" '+jg-python-swipe-to-def
      :n "SPC c t" #'+jg-python-pytest-dispatch
      :n "SPC \\"   #'+jg-python-popup-related-test
      )

(map! :map (python-mode-map python-ts-mode-map) ;; localleader
      :after python-mode
      :localleader
      :desc "ts-mode swap"     "TAB" #'+jg-python-swap-ts-mode
      :desc "Ruff Format"      "f" #'+jg-python-ruff-format
      :desc "Start Pydoc"      "p" #'+jg-python-start-pydoc
      :desc "Summarize"        "\\" #'+jg-python-summarize
      :desc "REPL"             "r" #'+jg-python/open-ipython-repl
      :desc "track"            ";" #'py-pdbtrack-toggle-stack-tracking
      :desc "breakpoint"       "b" #'+jg-python-breakpoint-line
      :desc "Disassemble"      "D" #'+jg-python-bytecode-dwim
      :desc "debug"            "d" (cmd! (setq jg-python-dev-mode (not jg-python-dev-mode))
                                          (message "Python Debug Mode: %s" jg-python-dev-mode))
      )

(map! :map (python-mode-map python-ts-mode-map) ;; localleader.imports
      :localleader
      :desc "Import View"            "i" #'+jg-python--import-view
      :desc "Sort imports"           "s" #'+jg-python-isort-diff
      (:prefix ("i" . "imports")
       :desc "Insert Import Snippet"  "I" #'+jg-python-import-snippet
       (:when (modulep! +isort)
         :desc "Sort imports"      "s" #'py-isort-buffer
         :desc "Sort region"       "r" #'py-isort-region
         )
       )
      )

(map! :map (python-mode-map python-ts-mode-map) ;; localleader.tests
      :localleader
      :prefix ("t" . "test")
      :desc "Test DWIM"       "f" #'python-pytest-file-dwim
      :desc "Test File"       "F" #'python-pytest-file
      :desc "Test Func DWIM"  "t" #'python-pytest-function-dwim
      :desc "Test Func"       "T" #'python-pytest-function
      :desc "Test repeat"     "r" #'python-pytest-repeat
      :desc "Test Popup"      "P" #'python-pytest-dispatch
      :desc "Test Popup"      "p" #'+jg-python-pytest
      :desc "Make Testfile"   "m" #'+jg-python-make-test-file
      )

(map! :map (python-mode-map python-ts-mode-map) ;; localleader.environment
      :localleader
      :prefix ("e" . "Environment")
      :desc "Choose Support" "c" #'+jg-python-support
      :desc "Current Support" "C" (cmd! (message "Current Python Support: %s" jg-python-last-chosen-support))
      :desc "install"     "i"    #'pipenv-install
      :desc "pipenv lock" "l"    #'pipenv-lock
      :desc "open module" "o"    #'pipenv-open
      :desc "run"         "r"    #'pipenv-run
      :desc "shell"       "s"    #'pipenv-shell
      :desc "uninstall"   "u"    #'pipenv-uninstall
      :desc "mamba activate" "m" #'micromamba-activate
      )

(map! :map (python-mode-map python-ts-mode-map) ;; localleader.coverage
      :localleader
      :desc "Coverage Refresh" "c" #'python-coverage-overlay-refresh
      :prefix ("C" . "Coverage")
      "o" #'python-coverage-overlay-mode
      "r" #'python-coverage-overlay-remove-all
      "RET" #'+jg-python-open-coverage-report
      :desc "Coverage Refresh" "R" #'python-coverage-overlay-refresh
      :desc "Toggle Coverage" "t" #'+jg-python-toggle-coverage-hook
      )

(map! :map inferior-python-mode-map
      :after python-mode
      "TAB" #'+jg-snippets-complete-or-snippet
      :n "DEL" #'counsel-shell-history--with-state-normal
      :localleader
      "q" #'comint-send-eof
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
      :desc "Disassemble Python" :n "d ? p" #'+jg-python-dired-dis
      :localleader
      :desc "Python site-packages" "f p" #'+jg-python-try-site-packages
      :desc "Make __init__.py"     "g i" (cmd! (+jg-dired-touch "__init__.py"))
      )

(map! :map jg-comint-mode-map
      :localleader
      :desc "PdbTrack"     ";" #'py-pdbtrack-toggle-stack-tracking
      :desc "Fold lines" "TAB" #'toggle-truncate-lines
      )

(map! :map py-test-minor-mode-map
      :n "RET" #'py-test-minor-function-dwim
      :i "RET" #'evil-ret
      :n "DEL" #'py-test-copy-current-test
      )

(map! :map python-pytest-mode-map
      :n "DEL" #'counsel-shell-history--with-state-normal
      :localleader
      "TAB" #'toggle-truncate-lines
      )
