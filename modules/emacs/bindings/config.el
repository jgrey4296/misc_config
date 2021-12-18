;;; config/default/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+vars")

(after! ibuffer
  (load! "+ibuffer-bindings")
)
(after! evil
  (load! "+leader-bindings")
  (load! "+misc-bindings")
  (load! "+help-map"
  )
(after! evil-ex
  (load! "+evil-ex-setup")
  )


(use-package general-mod
  :after general)
(use-package which-mod
  :after which-key)

(after! flycheck
  (map! :leader
        :desc "Flycheck" "!" flycheck-command-map
        :prefix "c"
        :desc "Flycheck" "!" flycheck-command-map
        )
  )

(defun jg-evil-bind-hook ()
  (load! "+evil-bindings")
  (load! "+evil-submap-bindings"))

(add-hook 'ibuffer-mode-hook #'+jg-ibuffer-update-hook)
(add-hook 'doom-first-input-hook #'jg-evil-bind-hook -100)
