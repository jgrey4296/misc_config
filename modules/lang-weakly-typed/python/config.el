;;; lang/jg-python/config.el -*- lexical-binding: t; -*-

(dlog! "Config JG Python")

(local-load! "+spec-defs")
(local-load! "+defs")
(defer-load! python-mode "+vars")
(defer-load! "+envs" "+lsp" "+cython")
(defer-load! jg-bindings-total "+bindings")

(advice-add 'python-shell-calculate-command :override #'+jg-python-shell-calculate-command)
(advice-add 'py--pdbtrack-get-source-buffer :override #'+jg-python-pdbtrack-silence)
(advice-add 'py--pdbtrack-track-stack-file  :override #'+jg-python-py--pdbtrack-track-stack-file)

(use-package! python
  :config
  (require 'python-mode)

  ;; setup python-ts-mode
  (add-hook! 'python-ts-mode-hook
             #'hs-minor-mode
             #'+python-use-correct-flycheck-executables-h
             #'abbrev-mode
             #'flycheck-mode
             #'maybe-py-test-minor-mode
             #'evil-collection-python-set-evil-shift-width
             #'general-insert-minor-mode
             #'+jg-python-auto-hide
             )

  (setq-hook! 'python-mode-hook
    tab-width                    py-indent-offset
    end-of-defun-function       #'python-nav-end-of-defun
    beginning-of-defun-function #'python-nav-beginning-of-defun

    indent-line-function        #'py-indent-line
    indent-region-function      #'py-indent-region
    lsp-diagnostic-filter       #'+jg-python-lsp-flycheck-filter

    jg-workspaces-find-buff-fn #'+jg-python-carousel-window-fn
    )
  )

(use-package! python-mode
  :after python
  :init
  (setq py-complete-function #'(lambda () nil)
        py-do-completion-p nil ;; nil
        py-company-pycomplete-p nil
        py-fast-process-p nil)
  :config

  ;;-- hooks
  (add-hook! 'python-mode-hook
             #'+python-use-correct-flycheck-executables-h
             #'er/add-python-mode-expansions
             #'evil-collection-python-set-evil-shift-width
             #'doom--setq-tab-width-for-python-mode-h
             #'tree-sitter!
             #'maybe-py-test-minor-mode
             #'general-insert-minor-mode
             #'abbrev-mode
             #'flycheck-mode
             )

  (defun jg-python-font-lock-mod-h ()
    (pushnew! python-font-lock-keywords
              '("^\s+return " (0 '(:background "mediumpurple4") t))
              '("^\s+def "    (0 '(:background "mediumpurple4") t))
              )
    )

  (add-hook! 'python-mode-hook :depth 100
             #'jg-python-font-lock-mod-h
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
    lsp-diagnostic-filter       #'+jg-python-lsp-flycheck-filter

    jg-workspaces-find-buff-fn #'+jg-python-carousel-window-fn
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
  :after anaconda-mode
  :commands 'company-anaconda
  )

(use-package! python-pytest
  :after python-mode
  )

(use-package! py-isort
  :commands py-isort-buffer
  :init
  (map! :after python
        :map python-mode-map
        :localleader
        (:prefix ("i" . "imports")
          :desc "Sort imports"      "s" #'py-isort-buffer
          :desc "Sort region"       "r" #'py-isort-region))
  )

(use-package! pyimport
  :after python-mode
  )

(use-package! pydoc
  :after python-mode
  ;; cmds pydoc, pydoc-at-point, pydoc-browse, pydoc-browse-kill
  )
