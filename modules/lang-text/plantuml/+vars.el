;;; +vars.el -*- lexical-binding: t; -*-

(after! flycheck-plantuml-executable
  (setq flycheck-plantuml-executable (executable-find "plantuml"))
  (add-to-list 'auto-mode-alist '("\\.pu" . plantuml-mode))
  )

(add-to-list 'auto-mode-alist '("\\.plantuml" . plantuml-mode))
