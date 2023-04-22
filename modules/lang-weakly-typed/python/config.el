;;; lang/jg-python/config.el -*- lexical-binding: t; -*-

(doom-log "Config JG Python")

(load! "util/+ivys")
(load! "+vars")
(load! "+funcs")
(load! "util/+hooks")
(load! "modes/+manifest-mode")
(after! jg-bindings-total
  (load! "+bindings")
  (load! "util/+nav")
  )
(load! "+advice")
(load! "util/+env")
(load! "modes/+derived-modes")

(use-package! python
  :config
  (require 'python-mode)
  )

(use-package! python-mode
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'"   . conf-mode)
  :init
  (when (modulep! +lsp)
    (add-hook 'python-mode-local-vars-hook #'lsp! 'append))
  (when (modulep! +tree-sitter)
    (add-hook 'python-mode-local-vars-hook #'tree-sitter! 'append))
  (when (executable-find "Microsoft.Python.LanguageServer")
    (set-eglot-client! 'python-mode '("Microsoft.Python.LanguageServer")))
  (set-docsets! '(python-mode inferior-python-mode) "Python 3" "NumPy" "SciPy" "Pandas")

  :config
  (set-repl-handler! 'python-mode #'+python/open-repl
    :persist t
    :send-region #'python-shell-send-region
    :send-buffer #'python-shell-send-buffer)

  (when (modulep! :ui modeline)
    (advice-add #'pythonic-activate :after-while #'+modeline-update-env-in-all-windows-h)
    (advice-add #'pythonic-deactivate :after #'+modeline-clear-env-in-all-windows-h))

  ;;-- hooks
  (add-hook! 'python-mode-hook
             #'anaconda-mode
             #'+python-use-correct-flycheck-executables-h
             #'doom-modeline-env-setup-python
             #'er/add-python-mode-expansions
             #'evil-collection-python-set-evil-shift-width
             #'doom--setq-tab-width-for-python-mode-h
             )

  ;; Always add auto-hide as the last thing
  (add-hook! 'python-mode-hook :depth 100
             #'+jg-python-outline-regexp-override-hook
             #'+jg-python-auto-hide
             )

  (setq-hook! 'python-mode-hook
    tab-width                    py-indent-offset
    end-of-defun-function       #'python-nav-end-of-defun
    beginning-of-defun-function #'python-nav-beginning-of-defun

    indent-line-function        #'python-indent-line
    indent-region-function      #'python-indent-region
    ;; indent-region-function      #'py-indent-region
    ;; indent-line-function        #'py-indent-line
    jg-company-activation-re jg-python-company-activation
    jg-company-kws           jg-python-company-kws
    )
  ;;-- end hooks

)

(use-package! anaconda-mode
  :defer t
  :init
  (setq anaconda-mode-installation-directory (concat doom-data-dir "anaconda/")
        anaconda-mode-eldoc-as-single-line t)

  (add-hook! 'python-mode-local-vars-hook :append
             #'+python-init-anaconda-mode-maybe-h)
  :config

  (add-hook 'anaconda-mode-hook #'anaconda-eldoc-mode)

  (add-hook! 'python-mode-hook
    (add-hook 'kill-buffer-hook #'+jg-python-auto-kill-anaconda-processes-h
              nil 'local))

  (add-hook 'anaconda-mode-hook #'evil-normalize-keymaps)
)

(use-package! company-anaconda
  :commands 'company-anaconda)

;;-- lsp

(use-package! lsp-pyright :after lsp-mode)

(use-package! lsp-jedi :defer)

(use-package! lsp-python-ms
  :disabled
  :unless (modulep! :lang python +pyright)
  :after (python lsp-mode)
  :config
  (setq lsp-python-ms-python-executable-cmd python-shell-interpreter)
  )

(use-package! lsp-jedi
  :disabled
  :ensure t
  :after lsp-mode
  :config
  (add-to-list 'lsp-enabled-clients 'jedi)
  )

;;-- end lsp

;;-- tests
;; (after! (origami python-origami)
;; (load! "util/jg-python-origami")
 ;;  (delq (assoc 'python-mode origami-parser-alist) origami-parser-alist)
 ;;  (add-to-list 'origami-parser-alist '(python-mode . +jg-origami-python-parser))
 ;;  )

(use-package! pyimport :after python-mode)

(use-package! py-isort
  :defer t
  :init
  (map! :after python
        :map python-mode-map
        :localleader
        (:prefix ("i" . "imports")
          :desc "Sort imports"      "s" #'py-isort-buffer
          :desc "Sort region"       "r" #'py-isort-region)))

(use-package! nose
  :commands nose-mode
  :preface (defvar nose-mode-map (make-sparse-keymap))
  :minor ("/test_.+\\.py$" . nose-mode)
  :config
  (set-yas-minor-mode! 'nose-mode)
  (when (featurep 'evil)
    (add-hook 'nose-mode-hook #'evil-normalize-keymaps))
  )

(use-package! python-pytest
  :commands python-pytest-dispatch
)

;;-- end tests

;;-- envs

(use-package! pipenv
  :commands pipenv-project-p
  :hook (python-mode . pipenv-mode)
  :init (setq pipenv-with-projectile nil)
  :config
  (set-eval-handler! 'python-mode
    '((:command . (lambda () python-shell-interpreter))
      (:exec (lambda ()
               (if-let* ((bin (executable-find "pipenv" t))
                         (_ (pipenv-project-p)))
                   (format "PIPENV_MAX_DEPTH=9999 %s run %%c %%o %%s %%a" bin)
                 "%c %o %s %a")))
      (:description . "Run Python script")))
  )

(use-package! pyvenv
  :after python
  :init
  (when (modulep! :ui modeline)
    (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
    (add-hook 'pyvenv-pre-deactivate-hooks #'+modeline-clear-env-in-all-windows-h))
  :config
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)
  (add-to-list 'global-mode-string
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
               'append)
  )

(use-package! conda
  :when (modulep! +conda)
  :after python
  :config
  ;; The location of your anaconda home will be guessed from a list of common
  ;; possibilities, starting with `conda-anaconda-home''s default value (which
  ;; will consult a ANACONDA_HOME envvar, if it exists).
  ;;
  ;; If none of these work for you, `conda-anaconda-home' must be set
  ;; explicitly. Afterwards, run M-x `conda-env-activate' to switch between
  ;; environments
  (or (cl-loop for dir in (list conda-anaconda-home
                                "~/.anaconda"
                                "~/.miniconda"
                                "~/.miniconda3"
                                "~/.miniforge3"
                                "~/anaconda3"
                                "~/miniconda3"
                                "~/miniforge3"
                                "~/opt/miniconda3"
                                "/usr/bin/anaconda3"
                                "/usr/local/anaconda3"
                                "/usr/local/miniconda3"
                                "/usr/local/Caskroom/miniconda/base"
                                "~/.conda")
               if (file-directory-p dir)
               return (setq conda-anaconda-home (expand-file-name dir)
                            conda-env-home-directory (expand-file-name dir)))
      (message "Cannot find Anaconda installation"))

  ;; integration with term/eshell
  (conda-env-initialize-interactive-shells)
  (after! eshell (conda-env-initialize-eshell))

  (add-to-list 'global-mode-string
               '(conda-env-current-name (" conda:" conda-env-current-name " "))
               'append))

(use-package! poetry
  :when (modulep! +poetry)
  :after python
  :init
  (setq poetry-tracking-strategy 'switch-buffer)
  (add-hook 'python-mode-hook #'poetry-tracking-mode))

(use-package! pip-requirements
  :defer t
  :config
  ;; HACK `pip-requirements-mode' performs a sudden HTTP request to
  ;;   https://pypi.org/simple, which causes unexpected hangs (see #5998). This
  ;;   advice defers this behavior until the first time completion is invoked.
  ;; REVIEW More sensible behavior should be PRed upstream.
  (defadvice! +python--init-completion-a (&rest args)
    "Call `pip-requirements-fetch-packages' first time completion is invoked."
    :before #'pip-requirements-complete-at-point
    (unless pip-packages (pip-requirements-fetch-packages)))
  (defadvice! +python--inhibit-pip-requirements-fetch-packages-a (fn &rest args)
    "No-op `pip-requirements-fetch-packages', which can be expensive."
    :around #'pip-requirements-mode
    (letf! ((#'pip-requirements-fetch-packages #'ignore))
      (apply fn args))))
;;-- end envs

;;-- cython

(use-package! cython-mode
  :when (modulep! +cython)
  :mode "\\.p\\(yx\\|x[di]\\)\\'"
  :config
  (setq cython-default-compile-format "cython -a %s")
)

(use-package! flycheck-cython
  :when (modulep! +cython)
  :when (modulep! :checkers syntax)
  :after cython-mode)

;;-- end cython
