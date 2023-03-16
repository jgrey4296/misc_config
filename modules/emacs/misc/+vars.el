;;; util/jg-misc/+vars.el -*- lexical-binding: t; -*-


;;-- evil-misc
(after! evil-quickscope
  (global-evil-quickscope-always-mode 1)
  )
;;-- end evil-misc

;;-- shellvars
(setq-default shell-default-shell 'shell
              shell-protect-eshell-prompt 0
              shell-enable-smart-eshell t
              )
;;-- end shellvars

;;-- diary
(setq-default diary-file (expand-file-name "diary" doom-user-dir))
;;-- end diary

;;-- minibuffer
(defvar +default-minibuffer-maps
  (append '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map
            read-expression-map)
          (cond ((modulep! :completion ivy)
                 '(ivy-minibuffer-map
                   ivy-switch-buffer-map))
                ((modulep! :completion helm)
                 '(helm-map
                   helm-rg-map
                   helm-read-file-map))))
  "A list of all the keymaps used for the minibuffer.")

;;-- end minibuffer

;;-- neotree
(after! neotree
  (push "^__pycache__$" neo-hidden-regexp-list)
  (push "^G\\(PATH\\|R?TAGS\\)$" neo-hidden-regexp-list)
  (push "^__init__.py$" neo-hidden-regexp-list)
  )
;;-- end neotree
