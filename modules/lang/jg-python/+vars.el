;;; lang/jg-python/+vars.el -*- lexical-binding: t; -*-


;-- General Python Vars
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
)
(modify-syntax-entry ?_ "_" python-mode-syntax-table)
;##-- end General Python Vars

;;-- projectile
(after! projectile
  (pushnew! projectile-project-root-files "setup.py" "requirements.txt"))
;;-- end projectile



;-- Personal Vars
(setq-default
              jg-python-dev-mode nil
              jg-python-dev-cmd "-X dev"

              jg-python-pycache-cmd "-X pycache_prefix=%s"
              jg-python-pycache-loc "~/.pycache"

              jg-python-docs-url           "https://docs.python.org/3/"
              jg-python-bibtex-parser-url  "https://bibtexparser.readthedocs.io/en/master/tutorial.html"
              jg-python-beautiful-soup-url "https://beautiful-soup-4.readthedocs.io/en/latest/"
              jg-conda-activate-cmd        "source /Volumes/documents/github/emacs_files/setup_files/bash_setup/python.bash && conda_activate_for_scripts %s"
              jg-python-last-chosen-support nil
              jg-python-import-block-end-re "^\\(__all__\\|[[:graph:]]+?\\s-+=\\|defun\\|class\\|if TYPE_CHECKING:\\)"
      )
;##-- end Personal Vars

;;-- flycheck
(setq-default flycheck--automatically-enabled-checkers (-concat flycheck--automatically-enabled-checkers '(python-pylint))
              flycheck--automatically-disabled-checkers '(python-compile python-pyright python-mypy)
              )
(push 'python-pylint flycheck-checkers)
(push ".mypy.ini" flycheck-python-mypy-ini)
;;-- end flycheck



;;-- popup
(setq jg-python-todo-popup-rules t
      jg-python-popup-rules
      '(
        ("^\\*pytest\\*"         :side bottom :ttl 5   :height 0.4 :quit t :select t)
        ("^\\*Anaconda\\*"       :side bottom :ttl 5   :height 0.4 :quit t :select nil)

        ))
;;-- end popup





;;-- obsolete
;; (defvar +python-ipython-command '("ipython" "-i" "--simple-prompt" "--no-color-info")
;;   "Command to initialize the ipython REPL for `+python/open-ipython-repl'.")
;; (defvar +python-jupyter-command '("jupyter" "console" "--simple-prompt")
;;   "Command to initialize the jupyter REPL for `+python/open-jupyter-repl'.")
;; (after! flycheck
;;   (flycheck-add-next-checker 'python-pylint '(t . python-pyright))
;;   )
;;-- end obsolete
