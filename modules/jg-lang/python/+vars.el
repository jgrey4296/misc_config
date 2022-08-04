;;; lang/jg-python/+vars.el -*- lexical-binding: t; -*-

;;-- personal vars
(setq-default jg-python-dev-mode nil
              jg-python-dev-cmd "-X dev"

              jg-python-pycache-cmd "-X pycache_prefix=%s"
              jg-python-pycache-loc "~/.pycache"

              jg-python-docs-url           "https://docs.python.org/3/"
              jg-python-lib-url-suffix     "library/%s.html"
              jg-python-bibtex-parser-url  "https://bibtexparser.readthedocs.io/en/master/tutorial.html"
              jg-python-beautiful-soup-url "https://beautiful-soup-4.readthedocs.io/en/latest/"
              jg-conda-activate-cmd        "source /Volumes/documents/github/emacs_files/setup_files/bash_setup/python.bash && conda_activate_for_scripts %s"
              jg-python-last-chosen-support nil
              jg-python-import-block-end-re "^\\(__all__\\|[[:graph:]]+?\\s-+=\\|def\\|class\\|if TYPE_CHECKING:\\)"
      )
;;-- end personal vars

;;-- general python
(after! python
  (setq-default python-indent-offset 4
                python-indent-guess-indent-offset nil
                python-shell-completion-native-enable t
                python-shell-virtualenv-root "~/anaconda3"
                python-pdbtrack-activate nil
                py-pdbtrack-do-tracking-p nil
                python-shell-completion-native-disabled-interpreters '("pypy")

                python-shell-interpreter "python3"
                python-shell-interpreter-args "-i"
                python-shell-interpreter-path-args "/Volumes/documents/github/emacs_files/modules/lang/jg-python/repl_startup.py "
                )
  (modify-syntax-entry ?_ "_" python-mode-syntax-table)
  )
;;-- end general python

;;-- projectile
(after! projectile
  (pushnew! projectile-project-root-files "setup.py" "requirements.txt"))
;;-- end projectile

;;-- flycheck
(after! flycheck
  (setq-default flycheck--automatically-enabled-checkers (-concat flycheck--automatically-enabled-checkers '(python-pylint))
                flycheck--automatically-disabled-checkers '(python-compile python-pyright python-mypy)
                )
  (push 'python-pylint flycheck-checkers)
  (push ".mypy.ini" flycheck-python-mypy-ini)
  )
;;-- end flycheck

;;-- popup
(setq jg-python-popup-rules
      '(("^\\*pytest\\*"         :side bottom :ttl 5   :height 0.4 :quit t :select t :priority 50)
        ("^\\*Anaconda\\*"       :side bottom :ttl 5   :height 0.4 :quit t :select nil :priority 50)
        ("^\\*Python\\*"         :side right  :ttl nil :width  0.5 :quit nil :select t :priority 50)
        ))
(after! jg-popup-init
  (+jg-ui-popup-add-rules 'python jg-python-popup-rules)
  )

;;-- end popup

;;-- file templates
(after! jg-file-templates
  (+jg-completion-add-file-templates
   'python
   '(("LICENSE$"        :trigger "__license-acab"   :mode text-mode :priority 100)
     ("pyproject.toml$" :trigger "__pyproject_toml" :mode python-mode)
     ("setup\\.cfg$"    :trigger "__setup_cfg"      :mode python-mode)
     ("__init__\\.py$"  :trigger "__init"           :mode python-mode)
     ("test_.+\\.py$"   :trigger "__tests"          :mode python-mode)
     ("_cli.+\\.py$"    :trigger "__cli"            :mode python-mode)
     ("conf\\.py$"      :trigger "__conf"           :mode python-mode)
     ("setup\\.py$"     :trigger "__setup"          :mode python-mode)
     ("\\.py$"          :trigger "__"               :mode python-mode :priority -99)
     (python-mode       :trigger "__" :priority -100)
     )
   )
  (+jg-completion-activate-file-templates)
  )
;;-- end file templates

;;-- obsolete
;; (defvar +python-ipython-command '("ipython" "-i" "--simple-prompt" "--no-color-info")
;;   "Command to initialize the ipython REPL for `+python/open-ipython-repl'.")
;; (defvar +python-jupyter-command '("jupyter" "console" "--simple-prompt")
;;   "Command to initialize the jupyter REPL for `+python/open-jupyter-repl'.")
;; (after! flycheck
;;   (flycheck-add-next-checker 'python-pylint '(t . python-pyright))
;;   )
;;-- end obsolete
