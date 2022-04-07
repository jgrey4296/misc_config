;;; lang/jg-python/config.el -*- lexical-binding: t; -*-

(doom-log "Config JG Python")

(after! python
  (load! "+vars")
)
(load! "+funcs")
(load! "+hooks")
(load! "+advice")
(after! evil
  (load! "+bindings")
  )

(use-package! pyimport
  :demand
  )
(use-package! lsp-jedi
  :defer)

(after! (dired pyvenv-mode)
    " Remove the annoying python-shell-setup advice "
    (add-transient-hook! 'dired-mode
      (map! :map dired-mode-map
        :localleader
        :n "v" 'pyvenv-activate
        )
      )
    )

(after! python
  (setq python-mode-hook nil)
  (add-hook! 'python-mode-hook #'outline-minor-mode
             #'+jg-python-outline-regexp-override-hook
             #'+python-use-correct-flycheck-executables-h
             #'+python-init-anaconda-mode-maybe-h
             #'+jg-python-auto-kill-conda-hook
             #'doom-modeline-env-setup-python
             #'er/add-python-mode-expansions
             #'evil-collection-python-set-evil-shift-width
             #'doom--setq-tab-width-for-python-mode-h
             #'yasnippet-snippets--fixed-indent
             )
  (setq-hook! 'python-mode-hook tab-width python-indent-offset)
)

(use-package-hook! anaconda-mode :post-config
  (+jg-python-conda-override)
  )

(after! evil
  (setq evil-fold-list (cons '((python-mode)
                               :close     +jg-python-close-class-defs
                               :close-all +jg-python-close-all-defs
                               :open      outline-toggle-children
                               :open-all  outline-show-all
                               :open-rec  outline-show-subtree
                               :toggle    outline-toggle-children
                               )
                             evil-fold-list))
  )

(after! lsp-mode
  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-disabled-clients 'pylsp)
  (add-to-list 'lsp-disabled-clients 'mspyls)
)
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
