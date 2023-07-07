;;; lang/jg-python/config.el -*- lexical-binding: t; -*-

(doom-log "Config JG Python")

(defer-load! python-mode "+vars")
(defer-load! "+spec-defs")
(defer-load! "+envs" "+lsp"  "+testing"  "+cython")
(defer-load! jg-bindings-total "+bindings")

(use-package! python
  :config
  (require 'python-mode)
  )

(use-package! python-mode
  :init
  (when (executable-find "Microsoft.Python.LanguageServer")
    (set-eglot-client! 'python-mode '("Microsoft.Python.LanguageServer")))

  (setq py-complete-function #'(lambda () nil)
        py-do-completion-p nil ;; nil
        py-company-pycomplete-p nil
        py-fast-process-p nil)
  :config

  (puthash "raise" #'+jg-python-exception-ivy jg-python-insert-ivys)
  (puthash "datetime" #'+jg-python-datetime-ivy jg-python-insert-ivys)
  (puthash "import" #'+jg-python-imports-ivy jg-python-insert-ivys)
  (puthash "fixtures" #'+jg-python-pytest-fixtures-ivy jg-python-insert-ivys)

  ;;-- hooks
  (add-hook! 'python-mode-hook
             #'+python-use-correct-flycheck-executables-h
             #'er/add-python-mode-expansions
             #'evil-collection-python-set-evil-shift-width
             #'doom--setq-tab-width-for-python-mode-h
             #'tree-sitter!
             #'maybe-py-test-minor-mode
             #'general-insert-minor-mode
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

    indent-line-function        #'py-indent-line
    indent-region-function      #'py-indent-region
    )
  ;;-- end hooks

)

(use-package! anaconda-mode
  :commands (anaconda-mode anaconda-mode-stop)
  :preface
  (setq anaconda-mode-installation-directory (concat doom-data-dir "anaconda/")
        anaconda-mode-eldoc-as-single-line t)
  (spec-handling-add! python-env
                      `(anaconda
                        (:support conda
                                  ,#'(lambda (state) (add-hook 'python-mode-hook #'anaconda-mode))
                                  ,#'(lambda (state) (anaconda-mode-stop) (remove-hook 'python-mode-hook #'anaconda-mode))
                                  )
                        (:teardown conda
                                   ,#'(lambda (state) (anaconda-mode-stop)
                                        (anaconda-eldoc-mode -1))
                                   )
                        )
                      )
  :config
  (add-hook! 'anaconda-mode-hook
             #'anaconda-eldoc-mode
             #'evil-normalize-keymaps
             )
)

(use-package! company-anaconda
  :commands 'company-anaconda
  )

(use-package! python-pytest
  :after python-mode
  :config
  (transient-append-suffix 'python-pytest-dispatch "-c" '("-f" "Logfile" +jg-python-test-logfile))
  (transient-append-suffix 'python-pytest-dispatch "D" '("q" "Quit" transient-quit-one))
  (transient-append-suffix 'python-pytest-dispatch "-x" '("--tc" "Trace Config" "--trace-config"))
  )
