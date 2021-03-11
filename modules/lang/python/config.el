;;; lang/python/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+vars")
(after! evil
  (load! "+bindings")
  )


;;
;;; Packages

(use-package! python
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode)
  :init
  (setq python-environment-directory doom-cache-dir
        python-indent-guess-indent-offset-verbose nil)

  (when (featurep! +lsp)
    (add-hook #'python-mode-local-vars-hook #'lsp!)
    ;; Use "mspyls" in eglot if in PATH
    (when (executable-find "Microsoft.Python.LanguageServer")
      (set-eglot-client! 'python-mode '("Microsoft.Python.LanguageServer"))))
  :config
  (set-repl-handler! 'python-mode #'+python/open-repl :persist t)
  (set-docsets! 'python-mode "Python 3" "NumPy" "SciPy")

  (set-ligatures! 'python-mode
    ;; Functional
    :def    "def"
    :lambda "lambda"
    ;; Types
    :null   "None"
    :true   "True"
    :false  "False"
    :int    "int"
    :str    "str"
    :float  "float"
    :bool   "bool"
    :tuple  "tuple"
    ;; Flow
    :not    "not"
    :in     "in"
    :not-in "not in"
    :and    "and"
    :or     "or"
    :for    "for"
    :return "return"
    :yield  "yield")

  ;; Stop the spam!
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  (add-hook! 'python-mode-hook #'outline-minor-mode)
  (add-hook! 'python-mode-hook #'+jg-personal-python-outline-regexp-override-hook)
  (add-hook! 'python-mode-hook #'+python-use-correct-flycheck-executables-h)
  (setq-hook! 'python-mode-hook tab-width python-indent-offset)

  (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens
  (sp-local-pair 'python-mode "'" nil
                 :unless '(sp-point-before-word-p
                           sp-point-after-word-p
                           sp-point-before-same-p))

  ;; Affects pyenv and conda
  (when (featurep! :ui modeline)
    (advice-add #'pythonic-activate :after-while #'+modeline-update-env-in-all-windows-h)
    (advice-add #'pythonic-deactivate :after #'+modeline-clear-env-in-all-windows-h))
)

(use-package! anaconda-mode
  :defer t
  :init
  (setq anaconda-mode-installation-directory (concat doom-etc-dir "anaconda/")
        anaconda-mode-eldoc-as-single-line t)

  (add-hook! 'python-mode-local-vars-hook :append
             #'+python-init-anaconda-mode-maybe-h)

  :config
  (set-company-backend! 'anaconda-mode '(company-anaconda))
  (set-lookup-handlers! 'anaconda-mode
    :definition #'anaconda-mode-find-definitions
    :references #'anaconda-mode-find-references
    :documentation #'anaconda-mode-show-doc)
  (set-popup-rule! "^\\*anaconda-mode" :select nil)

  (add-hook 'anaconda-mode-hook #'anaconda-eldoc-mode)
  (add-hook! 'python-mode-hook #'+python-auto-kill-hook-fn)

  (when (featurep 'evil)
    (add-hook 'anaconda-mode-hook #'evil-normalize-keymaps))
  )

(use-package! pyimport
  :defer t
  )

(use-package! py-isort
  :defer t
  )

(use-package! nose
  :commands nose-mode
  :preface (defvar nose-mode-map (make-sparse-keymap))
  :minor ("/test_.+\\.py$" . nose-mode)
  :config
  (set-popup-rule! "^\\*nosetests" :size 0.4 :select nil)
  (set-yas-minor-mode! 'nose-mode)
  (when (featurep 'evil)
    (add-hook 'nose-mode-hook #'evil-normalize-keymaps))
  )

(use-package! python-pytest
  :defer t
  )

(use-package! blacken
  :defer t
  )

(use-package! python-black
  :after python
  (add-hook! 'python-mode-hook #'python-black-on-save-mode)
  )

;;
;;; Environment management

(use-package! pipenv
  :commands pipenv-project-p
  :hook (python-mode . pipenv-mode)
  :init (setq pipenv-with-projectile nil)
  :config
  (set-eval-handler! 'python-mode
    '((:command . (lambda () python-shell-interpreter))
      (:exec (lambda ()
               (if-let* ((bin (executable-find "pipenv"))
                         (_ (pipenv-project-p)))
                   (format "PIPENV_MAX_DEPTH=9999 %s run %%c %%o %%s %%a" bin)
                 "%c %o %s %a")))
      (:description . "Run Python script")))
  )

(use-package! pyvenv
  :after python
  :init
  (when (featurep! :ui modeline)
    (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
    (add-hook 'pyvenv-pre-deactivate-hooks #'+modeline-clear-env-in-all-windows-h))
  :config
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)
  (add-to-list 'global-mode-string
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
               'append))

(use-package! pyenv-mode
  :when (featurep! +pyenv)
  :after python
  :config
  (pyenv-mode +1)
  (when (executable-find "pyenv")
    (add-to-list 'exec-path (expand-file-name "shims" (or (getenv "PYENV_ROOT") "~/.pyenv"))))
  (add-hook 'python-mode-local-vars-hook #'+python-pyenv-mode-set-auto-h)
  (add-hook 'doom-switch-buffer-hook #'+python-pyenv-mode-set-auto-h))

(use-package! conda
  :when (featurep! +conda)
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
                                "~/anaconda3"
                                "~/miniconda3"
                                "/usr/bin/anaconda3"
                                "/usr/local/anaconda3"
                                "/usr/local/miniconda3"
                                "/usr/local/Caskroom/miniconda/base")
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
  :when (featurep! +poetry)
  :after python)

(use-package! cython-mode
  :when (featurep! +cython)
  :mode "\\.p\\(yx\\|x[di]\\)\\'"
  :config
  (setq cython-default-compile-format "cython -a %s")
  )

(use-package! flycheck-cython
  :when (featurep! +cython)
  :when (featurep! :checkers syntax)
  :after cython-mode)

;;; LSP

(eval-when! (and (featurep! +lsp)
                 (not (featurep! :tools lsp +eglot)))

  (use-package! lsp-python-ms
    :unless (featurep! +pyright)
    :after lsp-mode
    :preface
    (after! python
      (setq lsp-python-ms-python-executable-cmd python-shell-interpreter)))

  (use-package! lsp-pyright
    :when (featurep! +pyright)
    :after lsp-mode))

(after! (dired pyvenv-mode)
    """ Remove the annoying python-shell-setup advice """
    (add-transient-hook! 'dired-mode
      (map! :map dired-mode-map
        :localleader
        :n "v" 'pyvenv-activate
        )
      )
    )
;; (after! (origami python-origami)
 ;;  (delq (assoc 'python-mode origami-parser-alist) origami-parser-alist)
 ;;  (add-to-list 'origami-parser-alist '(python-mode . +jg-origami-python-parser))
 ;;  )


