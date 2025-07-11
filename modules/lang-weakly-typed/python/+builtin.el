;;; +builtin-python.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;; Built in Python-mode
;; Provides:
;; - python-base-mode,
;; - python-mode
;; - python-ts-mode
;; - inferior-python-mode

(advice-add 'python-shell-calculate-command :override    #'+jg-python-shell-calculate-command)
(advice-add 'python-ts-mode                 :around      #'+jg-python-override-python-ts)
(advice-add 'python-ts-mode                 :after       #'python-ts-extend)

(use-package! python
  :config
  ;; internal python uses prefix: python-
  (defalias 'python-stock-mode #'python-mode)

  (modify-syntax-entry ?_ "_" python-mode-syntax-table)

  (add-hook! 'python-base-mode-hook
             #'abbrev-mode
             #'evil-collection-python-set-evil-shift-width
             ;; --
             #'librarian-insert-minor-mode
             #'maybe-py-test-minor-mode
             )

  (add-hook! 'python-mode-hook
             #'er/add-python-mode-expansions
             #'tree-sitter!
             )

  (add-hook! 'python-mode-hook :depth 100
             #'jg-python-font-lock-mod-h
             #'+jg-python-outline-regexp-override-hook
             #'outline-minor-mode
             )

  (add-hook! 'python-ts-mode-hook :depth 100
             #'treesit-fold-mode
             )


  (setq-hook! 'python-base-mode-hook
    lsp-diagnostic-filter                     #'+jg-python-lsp-flycheck-filter
    flycheck-pylintrc                         '("pylint.toml" "pyproject.toml")
    flycheck-python-ruff-config               '("ruff.toml" ".ruff.toml" "pyproject.toml")
    flycheck--automatically-enabled-checkers  '(python-ruff python-coverage)
    flycheck--automatically-disabled-checkers '(python-pylint python-flake8 python-pycompile python-compile python-pyright)
    flycheck-python-mypy-config               '("pyproject.toml")
    comment-start "# "
    jg-workspaces-find-buff-fn #'+jg-python-carousel-window-fn
    tab-width                    python-indent-offset
    )

  (add-hook! 'code-shy-minor-mode-hook #'+jg-python-auto-hide)
  )

(use-package! pythonic
  ;; Dependencies: python
  :commands (pythonic-activate pythonic-deactivate)
  :init
  (advice-add 'pythonic-activate :after-while #'+modeline-update-env-in-all-windows-h)
  (advice-add 'pythonic-deactivate :after #'+modeline-clear-env-in-all-windows-h)
  )

(use-package! micromamba
  ;; dependencies: pythonic
  :commands (micromamba-activate micromamba-deactivate)
  :init

  (speckler-add! lib-env ()
    :override t
    `(mamba
      :lang 'python
      :start #'jg-py-mamba-start-env
      :stop  #'jg-py-mamba-stop-env
      :modeline #'(lambda (state &rest args) (format "M:%s" (car-safe args)))
      )
    )
  )

;; --------------------------------------------------

(speckler-setq! python-ts ()
  python--treesit-keywords '("as" "assert" "async" "await" "break" "case"
                             "class" "continue" "def" "del" "elif" "else"
                             "except" "exec" "finally" "for" "from" "global"
                             "if" "import" "lambda" "match" "nonlocal" "pass"
                             "print" "raise" "return" "try" "while" "with"
                             "yield" "type" "and" "in" "is" "not" "or" "not in"
                             "is not")
  ;; python--treesit-builtin-types
  ;; python--treesit-builtins
  ;; python--treesit-type-regex
  ;; python--treesit-constants
  ;; python--treesit-operators
  ;; python--treesit-special-attributes
  ;; python--treesit-exceptions
  ;; -----
  ;; python--treesit-settings
  )
(speckler-add! fold ()
  `(python
    :modes python-mode
    :priority 25
    :triggers (:close     #'+jg-python-close-methods
               :close-all #'+jg-python-close-all-defs
               :open      #'outline-toggle-children
               :open-all  #'outline-show-all
               :open-rec  #'outline-show-subtree
               :toggle    #'outline-toggle-children
               )
    )
  `(python-ts
    :modes python-ts-mode
    :priority 25
    :triggers (:close     #'treesit-fold-close
               :close-all #'treesit-fold-close-all
               :open      #'treesit-fold-open
               :open-all  #'treesit-fold-open-all
               :open-rec  #'treesit-fold-open-recursively
               :toggle    #'treesit-fold-toggle
               )
    )
  )
(speckler-add! company ()
  '(python-mode (:mode company-gtags))
  '(python-ts-mode (:mode company-gtags))
  )
(speckler-add! docsets ()
  '((python-mode inferior-python-mode)
    "Python 3" "NumPy" "SciPy" "Pandas"
    )
  )
(speckler-add! compile-commands ()
  '(python
    #'+jg-python-get-commands
    #'+jg-python-solo-file-run)
  )
(speckler-add! repl ()
  '(python-mode
    :start +jg-python/open-repl
    :send  python-shell-send-region
    )
  )
(speckler-add! treesit-bin-override ()
  '(python :lib-base "python" :entry-func "tree_sitter_python")
  )
(speckler-add! treesit-source ()
  '(python        "git@github.com:tree-sitter/tree-sitter-python.git")
  )
(speckler-add! tree-sitter-lang ()
  '(python-mode    . python)
  '(python-ts-mode . python)
  )
(speckler-setq! python-builtin ()
  ;; Python settings
  python-indent-offset               4
  python-pdbtrack-activate           t
  python-shell-interpreter          "python3"
  python-shell-interpreter-args     "-i"

  python-shell-virtualenv-root (if (boundp 'conda-env-home-directory) conda-env-home-directory nil)
  python-indent-guess-indent-offset                     nil
  python-shell-completion-native-enable                 nil
  python-shell-completion-native-disabled-interpreters  '("pypy")
  expand-region-preferred-python-mode 'python-mode
)

(rx-let ((kwds (regexp (eval (s-join "\\|" jg-python-outline-keywords)))))
  (setq jg-python-outline-regexp
        (rx (* blank)
            (or "##--"
                (| "@" (+ word))
                kwds
                )
            )
        jg-python-outline-end-regexp ":[^\n]*\n"
        )
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    July 11, 2025
;; Modified:   July 11, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; +builtin-python.el ends here
