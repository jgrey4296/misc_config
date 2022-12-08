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
(after! python
  (load! "+derived-modes")
  )

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
