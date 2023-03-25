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


;;-- neotree
(after! neotree
  (push "^__pycache__$" neo-hidden-regexp-list)
  (push "^G\\(PATH\\|R?TAGS\\)$" neo-hidden-regexp-list)
  (push "^__init__.py$" neo-hidden-regexp-list)
  )
;;-- end neotree
