;;; lang/yaml/config.el -*- lexical-binding: t; -*-

(use-package! yaml-mode
  :commands yaml-mode
  :init
  :config
  (setq-hook! 'yaml-mode-hook tab-width yaml-indent-offset)
  (add-hook 'yaml-mode-hook #'tree-sitter!)
  )


(spec-handling-add! auto-modes
                    '(yaml
                       ("Procfile\\'" . yaml-mode)
                       ("\\.yml\\'" . yaml-mode)
                       ("\\.yaml\\'" . yaml-mode)
                      )
                    )
