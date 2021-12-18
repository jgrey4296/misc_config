;;; lang/jg-python/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(load! "+advice")
(after! evil
  (load! "+bindings")
  )
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
  (add-hook! 'python-mode-hook #'outline-minor-mode)
  (add-hook! 'python-mode-hook #'+jg-python-outline-regexp-override-hook)
  (add-hook! 'python-mode-hook #'+python-use-correct-flycheck-executables-h)
  (setq-hook! 'python-mode-hook tab-width python-indent-offset)


  (setq evil-fold-list (cons '((python-mode)
                               :close     +jg-python-close-class-defs
                               :close-all +jg-python-close-all-defs
                               :open     outline-toggle-children
                               :open-all outline-show-all
                               :open-rec outline-show-subtree
                               :toggle outline-toggle-children
                               )
                             evil-fold-list))

  )

(use-package! pyimport
  :demand
  )

(use-package-hook! anaconda-mode :post-config
  (+jg-python-conda-override)
  )

;; (after! (origami python-origami)
 ;;  (delq (assoc 'python-mode origami-parser-alist) origami-parser-alist)
 ;;  (add-to-list 'origami-parser-alist '(python-mode . +jg-origami-python-parser))
 ;;  )
