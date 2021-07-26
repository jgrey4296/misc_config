;;; config.el -*- lexical-binding: t; -*-

(load! "+ob-plantuml")
(after! evil
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

(after! erlang
  ;; (also has a load path set in root el file)
  (setq erlang-root-dir "/usr/local/opt/erlang"
        exec-path (cons "/usr/local/opt/erlang/bin" exec-path)
        )
  )

(use-package gradle-mode)
(use-package groovy-mode)

;; Misc AI Languages:
(use-package agentspeak-mode)
(use-package ceptre-mode)
(use-package instal-mode)
(use-package jacamo-mode)
(use-package versu-mode)
