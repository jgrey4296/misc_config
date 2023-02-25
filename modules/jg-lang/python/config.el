;;; lang/jg-python/config.el -*- lexical-binding: t; -*-

(doom-log "Config JG Python")

(load! "+manifest-mode")
(load! "+vars")
(load! "+funcs")
(load! "+hooks")
(after! jg-bindings-total
  (load! "+bindings")
  (load! "+nav")
  )
(load! "+advice")
(load! "+env")
(load! "+derived-modes")

(use-package-hook! python :post-config
  (require 'python-mode)
  )

(use-package! python-mode
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode)

  :init
  (setq python-environment-directory doom-cache-dir
        python-indent-guess-indent-offset-verbose nil)

  (when (modulep! +lsp)
    (add-hook 'python-mode-local-vars-hook #'lsp! 'append))
  (when (modulep! +tree-sitter)
    (add-hook 'python-mode-local-vars-hook #'tree-sitter! 'append))

  :config
  (setq python-mode-hook nil
        python-mode-local-vars-hook nil)

  (add-hook! 'python-mode-hook
             #'doom-modeline-env-setup-python
             #'er/add-python-mode-expansions
             #'evil-collection-python-set-evil-shift-width
             #'doom--setq-tab-width-for-python-mode-h
             )

  (defun +jg-python-customisation-hook ()
    ;; (put 'defun 'bounds-of-thing-at-point '+jg-python-def-bounds)
    (add-hook 'jg-text-whitespace-clean-hook '+jg-python-cleanup-ensure-newline-before-def 5 t)
    (add-hook 'jg-text-whitespace-clean-hook 'delete-trailing-whitespace 10 t)
    (add-hook 'jg-text-whitespace-clean-hook '+jg-text-cleanup-whitespace 20 t)
    )

  ;; Always add auto-hide as the last thing
  (add-hook! 'python-mode-hook :depth 100
             #'anaconda-mode
             #'+jg-python-outline-regexp-override-hook
             #'+jg-python-customisation-hook
             #'+jg-python-auto-hide
             )
  (setq-hook! 'python-mode-hook
    tab-width                    python-indent-offset
    end-of-defun-function       'python-nav-end-of-defun
    beginning-of-defun-function 'python-nav-beginning-of-defun
    indent-region-function      'python-indent-region
    indent-line-function        'python-indent-line
    )

  (set-repl-handler! 'python-mode #'+python/open-repl
    :persist t
    :send-region #'python-shell-send-region
    :send-buffer #'python-shell-send-buffer)

  (set-docsets! '(python-mode inferior-python-mode) "Python 3" "NumPy" "SciPy" "Pandas")

  (when (modulep! :ui modeline)
    (advice-add #'pythonic-activate :after-while #'+modeline-update-env-in-all-windows-h)
    (advice-add #'pythonic-deactivate :after #'+modeline-clear-env-in-all-windows-h))
)

(use-package-hook! anaconda-mode :post-config
  (+jg-python-conda-binding-override)
  (set-company-backend! 'anaconda-mode 'company-anaconda)
  )

(use-package! company-anaconda
  :commands 'company-anaconda)

(use-package! lsp-pyright
  :after lsp-mode
  :init
  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-disabled-clients 'pylsp)
  (add-to-list 'lsp-disabled-clients 'mspyls)
)

(use-package! pyimport :demand)

(use-package! lsp-jedi :defer)

;; (use-package! lsp-python-ms
;;   :unless (modulep! :lang python +pyright)
;;   :after lsp-mode
;;   :preface
;;   (after! python
;;     (setq lsp-python-ms-python-executable-cmd python-shell-interpreter)))

;; (use-package! lsp-jedi
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (add-to-list 'lsp-enabled-clients 'jedi)
;;   )

;; (after! (origami python-origami)
 ;;  (delq (assoc 'python-mode origami-parser-alist) origami-parser-alist)
 ;;  (add-to-list 'origami-parser-alist '(python-mode . +jg-origami-python-parser))
 ;;  )
