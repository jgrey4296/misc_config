;;; lang/jg-python/config.el -*- lexical-binding: t; -*-

(doom-log "Config JG Python")

(load! "+manifest-mode")
(after! python
  (load! "+vars")
)
(load! "+funcs")
(load! "+hooks")
(after! evil
  (load! "+bindings")
  (load! "+nav")
  )
(load! "+env")
(load! "+file-templates")

(use-package! pyimport
  :demand
  )
(use-package! lsp-jedi
  :defer)


(use-package-hook! python :post-config
  (setq python-mode-hook nil)
  (setq python-mode-local-vars-hook nil)
  (add-hook! 'python-mode-hook #'outline-minor-mode
             #'+jg-python-outline-regexp-override-hook
             #'+python-use-correct-flycheck-executables-h
             #'doom-modeline-env-setup-python
             #'er/add-python-mode-expansions
             #'evil-collection-python-set-evil-shift-width
             #'doom--setq-tab-width-for-python-mode-h
             )
  ;; Always add auto-hide as the last thing
  (add-hook! 'python-mode-hook :depth 100
             '+jg-python-auto-hide
             )
  (setq-hook! 'python-mode-hook tab-width python-indent-offset)
)

(use-package-hook! anaconda-mode :post-config
  (+jg-python-conda-binding-override)
  )

(use-package! lsp-pyright
  :after lsp-mode
  :init
  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-disabled-clients 'pylsp)
  (add-to-list 'lsp-disabled-clients 'mspyls)
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

  ;; (use-package! lsp-python-ms
  ;;   :unless (featurep! +pyright)
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
