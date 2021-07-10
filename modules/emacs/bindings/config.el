;;; config/default/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+vars")

(after! ibuffer
  (load! "+ibuffer-bindings")
)
(after! evil
  (load! "+leader-bindings")
  (load! "+misc-bindings")
  )
(after! evil-ex
  (load! "+evil-ex-setup")
  )


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

(add-hook! ibuffer-mode #'+jg-ibuffer-update-hook)
(add-hook! doom-first-input #'jg-evil-bind-hook)
