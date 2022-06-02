;;; lang/jg-python/+vars.el -*- lexical-binding: t; -*-
(setq-default python-indent-offset 4
              python-indent-guess-indent-offset nil
              python-shell-interpreter-args "-i"
              python-shell-interpreter "python"
              python-shell-completion-native-enable t
              python-shell-virtualenv-root "~/anaconda3"
              python-shell--interpreter nil
              python-shell--interpreter-args nil
              flycheck--automatically-enabled-checkers (-concat flycheck--automatically-enabled-checkers '(python-pylint python-pyright python-mypy))
              flycheck--automatically-disabled-checkers '(python-compile)
              python-pdbtrack-activate nil
              py-pdbtrack-do-tracking-p nil
              python-shell-completion-native-disabled-interpreters '("pypy")
              )

(setq jg-python-docs-url           "https://docs.python.org/3/"
      jg-python-bibtex-parser-url  "https://bibtexparser.readthedocs.io/en/master/tutorial.html"
      jg-python-beautiful-soup-url "https://beautiful-soup-4.readthedocs.io/en/latest/"
      jg-conda-activate-cmd        "source /Volumes/documents/github/emacs_files/setup_files/bash_setup/python.bash && conda_activate_for_scripts %s"
      )


(defvar +python-ipython-command '("ipython" "-i" "--simple-prompt" "--no-color-info")
  "Command to initialize the ipython REPL for `+python/open-ipython-repl'.")
(defvar +python-jupyter-command '("jupyter" "console" "--simple-prompt")
  "Command to initialize the jupyter REPL for `+python/open-jupyter-repl'.")

(modify-syntax-entry ?_ "_" python-mode-syntax-table)
(push 'python-pylint flycheck-checkers)
(push ".mypy.ini" flycheck-python-mypy-ini)


(after! flycheck
  (flycheck-add-next-checker 'python-pylint '(t . python-pyright))
  )


(after! projectile
  (pushnew! projectile-project-root-files "setup.py" "requirements.txt"))
