;;; config/default/config.el -*- lexical-binding: t; -*-

(load! "+vars")

(after! (evil faster-whichkey)
  (load! "+minibuffer-bindings")
  (load! "+leader-bindings")
  (load! "submaps/+evil-bindings")
  (load! "+misc-bindings")
  (provide 'jg-bindings-total)
  )

(use-package! faster-whichkey
  :after (which-key general)
  )


(after! evil-escape
  (setq evil-escape-inhibit-functions nil ;; '(evil-ex-p)
        evil-escape-excluded-states '(normal multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15
        )

  ;; To fix interference between evil-escape and evil-ex
  (defun jg-evil-escape-fix2 (&rest args)
    nil
    )
  (advice-add 'evil-escape--insert-func :override #'jg-evil-escape-fix2)
  (advice-add 'evil-escape--delete-func :override #'jg-evil-escape-fix2)
  )
