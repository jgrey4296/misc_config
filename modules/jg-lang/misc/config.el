;;; config.el -*- lexical-binding: t; -*-

(after! jg-bindings-total
  (load! "+bindings")
  )

(after! flycheck-plantuml-executable
  (setq flycheck-plantuml-executable (executable-find "plantuml"))
  )

(after! ob-plantuml
  (advice-add #'org-babel-execute:plantuml
              :override #'+jg-misc-ob-plantuml-execute
              '((depth . -100)))
  )


(use-package gradle-mode)
(use-package groovy-mode)
