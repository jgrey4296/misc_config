;;; lang/jg-python/+vars.el -*- lexical-binding: t; -*-
(setq-default python-indent-offset 4
              python-indent-guess-indent-offset nil
              python-shell-interpreter-args "-i"
              python-shell-interpreter "python3"
              python-shell-completion-native-enable t
              python-shell-virtualenv-root "~/anaconda3"
              python-pdbtrack-activate nil
              py-pdbtrack-do-tracking-p nil
              python-shell-completion-native-disabled-interpreters '("pypy")

              python-shell-interpreter-path-args "-c \"import sys; sys.path = [%s] + sys.path; print(sys.path)\""

              jg-python-dev-mode nil
              jg-python-dev-cmd "-X dev"

              jg-python-pycache-cmd "-X pycache_prefix=%s"
              jg-python-pycache-loc "~/.pycache"


              flycheck--automatically-enabled-checkers (-concat flycheck--automatically-enabled-checkers '(python-pylint))
              flycheck--automatically-disabled-checkers '(python-compile python-pyright python-mypy)
              )

(setq jg-python-docs-url           "https://docs.python.org/3/"
      jg-python-bibtex-parser-url  "https://bibtexparser.readthedocs.io/en/master/tutorial.html"
      jg-python-beautiful-soup-url "https://beautiful-soup-4.readthedocs.io/en/latest/"
      jg-conda-activate-cmd        "source /Volumes/documents/github/emacs_files/setup_files/bash_setup/python.bash && conda_activate_for_scripts %s"
      jg-python-last-chosen-support nil
      )

(setq-default jg-python-fold-block-start-re "^##-- \\(\\sw+\\)"
              jg-python-fold-block-end-re   "^##-- End %s"
              jg-python-import-block-end-re "^\\(__all__\\|[[:ascii:]]+?\\s-+=\\|defun\\|class\\)"
              )


;; (defvar +python-ipython-command '("ipython" "-i" "--simple-prompt" "--no-color-info")
;;   "Command to initialize the ipython REPL for `+python/open-ipython-repl'.")
;; (defvar +python-jupyter-command '("jupyter" "console" "--simple-prompt")
;;   "Command to initialize the jupyter REPL for `+python/open-jupyter-repl'.")

(modify-syntax-entry ?_ "_" python-mode-syntax-table)
(push 'python-pylint flycheck-checkers)
(push ".mypy.ini" flycheck-python-mypy-ini)


;; (after! flycheck
;;   (flycheck-add-next-checker 'python-pylint '(t . python-pyright))
;;   )


(after! projectile
  (pushnew! projectile-project-root-files "setup.py" "requirements.txt"))
