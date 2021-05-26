;;; config/default/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+vars")
(load! "+which-key-update")

(after! ibuffer
  (load! "+ibuffer-bindings")
)
(after! evil
  (load! "+leader-bindings")
  (load! "+leaderless-bindings")
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
  (load! "+evil-bindings-2"))

(add-hook! ibuffer-mode #'+jg-ibuffer-update-hook)
(add-hook! doom-first-input #'jg-evil-bind-hook)
