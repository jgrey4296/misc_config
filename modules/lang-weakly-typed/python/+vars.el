;;; lang/jg-python/+vars.el -*- lexical-binding: t; -*-

(defvar +python-ipython-command '("ipython" "-i" "--simple-prompt" "--no-color-info")
  "Command to initialize the ipython REPL for `+python/open-ipython-repl'.")

(defvar +python-jupyter-command '("jupyter" "console" "--simple-prompt")
  "Command to initialize the jupyter REPL for `+python/open-jupyter-repl'.")

(after! projectile
  (pushnew! projectile-project-root-files "pyproject.toml" "requirements.txt" "setup.py"))

;;-- personal vars
(setq-default jg-python-dev-mode nil
              jg-python-dev-cmd "-X dev"

              jg-python-docs-url           "https://docs.python.org/3/"
              jg-python-lib-url-suffix     "library/%s.html"
              jg-python-bibtex-parser-url  "https://bibtexparser.readthedocs.io/en/master/tutorial.html"
              jg-python-beautiful-soup-url "https://beautiful-soup-4.readthedocs.io/en/latest/"
              jg-conda-activate-cmd        "source $HOME/.doom.d/terminal/bash/conda.bash && activate %s"
              jg-python-last-chosen-support nil
              jg-python-import-block-end-re "^\\(__all__\\|[[:graph:]]+?\\s-+=\\|def\\|class\\|if TYPE_CHECKING:\\)"

              jg-python-summary-buffer "*Python-Summary*"

              expand-region-preferred-python-mode 'python-mode
      )
;;-- end personal vars

;;-- rotate text
(set-rotate-patterns! 'python-mode
  :symbols '(("True" "False")

             )
  )

;;-- end rotate text

;;-- general python
(after! python-mode
  (setq-default python-indent-offset 4
                python-indent-guess-indent-offset nil
                python-shell-completion-native-enable nil
                python-shell-virtualenv-root (expand-file-name  "~/anaconda")
                python-pdbtrack-activate t
                py-pdbtrack-do-tracking-p t
                python-shell-completion-native-disabled-interpreters '("pypy")

                ;; python-shell-interpreter "python3"
                python-shell-interpreter "python3"
                python-shell-interpreter-args "-i"
                ;; python-shell-interpreter-args `("-X" ,(format "pycache_prefix=%s" (expand-file-name  "~/.pycache")))
                python-shell-interpreter-path-args (doom-module-expand-path :jg-lang 'python "repl_startup.py ")

                py-use-font-lock-doc-face-p t
                py-fontify-shell-buffer-p t
                )
  (modify-syntax-entry ?_ "_" python-mode-syntax-table)
  )
;;-- end general python

;;-- outline
(after! python-mode
  (setq jg-python-outline-regexp
        (rx-let ((kwds (regexp (eval (s-join "\\|" py-outline-mode-keywords))))
                 )
        (rx (* blank)
            (or "##--"
                (| "@" (+ word))
                kwds
                )
            )
        )
        jg-python-outline-end-regexp ":[^\n]*\n"
        )
)
;;-- end outline

;;-- projectile
(after! projectile
  (pushnew! projectile-project-root-files "setup.py" "requirements.txt")
  (projectile-register-project-type 'jg-python-project '("pyproject.toml")
                                    :project-file "pyproject.toml"
                                    :configure "pip install -e %s"
                                    :test "python -m unittest discover -v -p test_*.py"
                                    :test-dir '(lambda (x) (f-join x "__tests"))
                                    :test-prefix "test_"
                                    :related-files-fn #'+jg-python-related-files-fn
                                    )

  )
;;-- end projectile

;;-- flycheck
(after! flycheck
  (setq flycheck-pylintrc '("pylint.toml" "pyproject.toml"))
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
        ("^\\*Python-Summary\\*" :side right  :ttl nil :width  0.2 :quit t  :select nil :priority 50)
        ))
(after! jg-popup-init
  (+jg-ui-popup-add-rules 'python jg-python-popup-rules)
  )

;;-- end popup

;;-- file templates
(after! jg-completion-templates
  (+jg-completion-add-file-templates
   'python
   '(("LICENSE$"        :trigger "__license-acab"   :mode text-mode :priority 100)
     ("pyproject.toml$" :trigger "__pyproject"      :mode conf-toml-mode)
     ("setup\\.cfg$"    :trigger "__setup_cfg"      :mode python-mode)
     ("__init__\\.py$"  :trigger "__init"           :mode python-mode)
     ("test_.+\\.py$"   :trigger "__tests"          :mode python-mode)
     ("cli_.+\\.py$"    :trigger "__cli"            :mode python-mode)
     ("conf\\.py$"      :trigger "__conf"           :mode python-mode)
     ("setup\\.py$"     :trigger "__setup"          :mode python-mode)
     ("dooter\\.py$"    :trigger "__doot"           :mode python-mode)
     ("SConstruct"      :trigger "__sconstruct"     :mode python-mode)
     ("SConscript"      :trigger "__sconscript"     :mode python-mode)
     ("\\.py$"          :trigger "__"               :mode python-mode :priority -99)
     (python-mode       :trigger "__" :priority -100)
     )
   )
  )
;;-- end file templates

;;-- browse providers
(after! jg-browse-providers
  (pushnew! jg-browse-providers-alist
            '("Python" "https://docs.python.org/3/search.html?q=%s&check_keywords=yes&area=default")
            '("Pypi"   "https://pypi.org/search/?q=%s")

            )
  )

;;-- end browse providers

;;-- fold
(setq jg-python-fold-spec '((python-mode)
                            :close     +jg-python-close-class-defs
                            :close-all +jg-python-close-all-defs
                            :open      outline-toggle-children
                            :open-all  outline-show-all
                            :open-rec  outline-show-subtree
                            :toggle    outline-toggle-children
                            )
      )
(after! jg-fold-specs
  (push jg-python-fold-spec evil-fold-list)
  )
;;-- end fold

;;-- obsolete
;; (defvar +python-ipython-command '("ipython" "-i" "--simple-prompt" "--no-color-info")
;;   "Command to initialize the ipython REPL for `+python/open-ipython-repl'.")
;; (defvar +python-jupyter-command '("jupyter" "console" "--simple-prompt")
;;   "Command to initialize the jupyter REPL for `+python/open-jupyter-repl'.")
;; (after! flycheck
;;   (flycheck-add-next-checker 'python-pylint '(t . python-pyright))
;;   )
;;-- end obsolete

;;-- smartparens
(after! smartparens-python
  (sp-with-modes 'python-mode
    ;; Automatically close f-strings
    (sp-local-pair "f\"" "\"")
    (sp-local-pair "f\"\"\"" "\"\"\"")
    (sp-local-pair "f'''" "'''")
    (sp-local-pair "f'" "'"))
  ;; Original keybind interferes with smartparens rules
  (define-key python-mode-map (kbd "DEL") nil)
  ;; Interferes with the def snippet in doom-snippets
  ;; TODO Fix this upstream, in doom-snippets, instead
  (setq sp-python-insert-colon-in-function-definitions nil))
;;-- end smartparens