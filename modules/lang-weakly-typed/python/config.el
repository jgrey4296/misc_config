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
  (when (executable-find "Microsoft.Python.LanguageServer")
    (set-eglot-client! 'python-mode '("Microsoft.Python.LanguageServer")))

  (spec-handling-add! python-env nil
                      '(flycheck
                        (:support . flycheck)
                        )
                      )
  :config

  ;;-- hooks
  (add-hook! 'python-mode-hook
             #'+python-use-correct-flycheck-executables-h
             #'er/add-python-mode-expansions
             #'evil-collection-python-set-evil-shift-width
             #'doom--setq-tab-width-for-python-mode-h
             #'+jg-python-handle-env!
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
    jg-company-activation-re    jg-python-company-activation
    jg-company-kws              jg-python-company-kws
    )
  ;;-- end hooks

)

(use-package! anaconda-mode
  :defer t
  :init
  (setq anaconda-mode-installation-directory (concat doom-data-dir "anaconda/")
        anaconda-mode-eldoc-as-single-line t)
  (spec-handling-add! python-env nil
                      '(anaconda
                        (:support . conda)
                        )
                      )
  :config
  (add-hook! 'anaconda-mode-hook
             #'anaconda-eldoc-mode
             #'evil-normalize-keymaps
             )
)

(use-package! company-anaconda
  :commands 'company-anaconda)

;;-- lsp

(use-package! lsp-pyright
  :after lsp-mode
  :when (modulep! +lsp)
  :init
  ;; (add-to-list 'lsp-enabled-clients 'pyright)
  (spec-handling-add! python-env nil
                      '(pyright
                        (:support . lsp)
                        )
                      )
  )

(use-package! lsp-python-ms
  :disabled
  :unless (modulep! +pyright)
  :after (python lsp-mode)
  :init
  (add-to-list 'lsp-disabled-clients 'python-ms)
  (spec-handling-add! python-env nil
                      '(lsp-ms
                        (:support . lsp)
                        )
                      )
  :config
  (setq lsp-python-ms-python-executable-cmd python-shell-interpreter)
  )

(use-package! lsp-jedi
  :disabled
  :ensure t
  :after lsp-mode
  :init
  (add-to-list 'lsp-disabled-clients 'jedi)
  (spec-handling-add! python-env nil
                      '(lsp-jedi
                        (:support . lsp)
                        )
                      )

  )

;;-- end lsp

;;-- tests

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
  :preface

(defvar nose-mode-map (make-sparse-keymap))
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
(use-package! pythonic
  :defer t
  :init
  (spec-handling-add! python-env nil
                      '(pythonic
                        (:activator . pythonic)
                        )
                      )
  (advice-add #'pythonic-activate :after-while #'+modeline-update-env-in-all-windows-h)
  (advice-add #'pythonic-deactivate :after #'+modeline-clear-env-in-all-windows-h)
  )

(use-package! pipenv
  :commands pipenv-project-p
  :init
  (setq pipenv-with-projectile nil)
  (spec-handling-add! python-env nil
                      '(pipenv
                        (:activator . pipenv)
                        )
                      )
  )

(use-package! pyvenv
  :after python
  :init
  (when (modulep! :ui modeline)
    (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
    (add-hook 'pyvenv-pre-deactivate-hooks #'+modeline-clear-env-in-all-windows-h))
  (spec-handling-add! python-env nil
                      '(pyvenv
                        (:activator . pyvenv)
                        )
                      )
  )

(use-package! conda
  :when (modulep! +conda)
  :after python
  :init
  (spec-handling-add! python-env nil
                      '(conda_el
                        (:activator . conda)
                        )
                      )

  :config
  (setq conda-anaconda-home (or (getenv "ANACONDA_HOME") "/usr/local/anaconda3"))
  (setq conda-env-home-directory (f-join conda-anaconda-home "envs"))
)

(use-package! poetry
  :after python
  :init
  (spec-handling-add! python-env nil
                      '(poetry
                        (:activator . poetry)
                        )
                      )
  )

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

;; (after! (origami python-origami)
;; (load! "util/jg-python-origami")
 ;;  (delq (assoc 'python-mode origami-parser-alist) origami-parser-alist)
 ;;  (add-to-list 'origami-parser-alist '(python-mode . +jg-origami-python-parser))
 ;;  )

(spec-handling-new! python-env jg-python-env-registered nil append
                    val
                    )
