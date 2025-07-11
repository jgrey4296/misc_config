;;; +external-python.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;; External python mode, provides:
;; - python-mode
;; - py-shell-mode
;; - py-autocompletion-mode

(advice-add 'py--pdbtrack-get-source-buffer :override    #'+jg-python-pdbtrack-silence)
(advice-add 'py--pdbtrack-track-stack-file  :override    #'+jg-python-py--pdbtrack-track-stack-file)

(use-package! python-mode
  :init
  :config
  (modify-syntax-entry ?_ "_" python-mode-syntax-table)
  (defalias 'python-external-mode #'python-mode)

  (add-hook! 'python-mode-hook
             #'abbrev-mode
             #'er/add-python-mode-expansions
             #'evil-collection-python-set-evil-shift-width
             ;; --
             #'librarian-insert-minor-mode
             #'maybe-py-test-minor-mode
             #'tree-sitter!
             )

  (add-hook! 'python-mode-hook :depth 100
             #'jg-python-font-lock-mod-h
             #'+jg-python-outline-regexp-override-hook
             #'outline-minor-mode
             )

  (setq-hook! 'python-mode-hook
    lsp-diagnostic-filter                     #'+jg-python-lsp-flycheck-filter
    flycheck-pylintrc                         '("pylint.toml" "pyproject.toml")
    flycheck-python-ruff-config               '("ruff.toml" ".ruff.toml" "pyproject.toml")
    flycheck--automatically-enabled-checkers  '(python-ruff python-coverage)
    flycheck--automatically-disabled-checkers '(python-pylint python-flake8 python-pycompile python-compile python-pyright)
    flycheck-python-mypy-config               '("pyproject.toml")
    comment-start "# "
    jg-workspaces-find-buff-fn #'+jg-python-carousel-window-fn
    tab-width                    py-indent-offset
    )

  (rx-let ((kwds (regexp (eval (s-join "\\|" py-outline-mode-keywords)))))
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
  )

;; --------------------------------------------------

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
  )
(speckler-add! company ()
  '(python-mode (:mode company-gtags))
  )
(speckler-setq! python-external ()
  py--imenu-create-index-p              nil
  py-company-pycomplete-p               nil
  py-complete-function #'(lambda () nil)
  py-debug-p                            nil
  py-defun-use-top-level-p              nil
  py-do-completion-p                    nil
  py-empty-comment-line-separates-paragraph-p t
  py-fast-process-p                     nil
  py-font-lock-defaults-p               t
  py-fontify-shell-buffer-p             t
  py-guess-py-install-directory-p       nil
  py-hide-show-minor-mode-p             nil
  py-indent-offset 4
  py-load-pymacs-p                      nil
  py-load-skeletons-p                   nil
  py-outline-minor-mode-p               nil
  py-pdbtrack-do-tracking-p     t
  py-python-command             "python3"
  py-python-command-args        '("-i")
  py-sexp-use-expression-p              nil
  py-shell-virtualenv-root      (if (boundp 'conda-env-home-directory) conda-env-home-directory nil)
  py-split-window-on-execute    t
  py-trailing-whitespace-smart-delete-p nil
  py-use-font-lock-doc-face-p           t
  ;; Python settings
  expand-region-preferred-python-mode 'python-mode
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
;;; +external-python.el ends here
