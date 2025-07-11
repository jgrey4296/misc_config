;;; lang/yaml/config.el -*- lexical-binding: t; -*-

(use-package! yaml-mode
  :commands yaml-mode
  :init
  :config
  (setq-hook! 'yaml-mode-hook tab-width yaml-indent-offset)
  (add-hook 'yaml-mode-hook #'tree-sitter!)
  (add-hook 'yaml-mode-hook #'librarian-insert-minor-mode)

  (map! :map yaml-mode-map
        :desc "General Insert"         :n "|" #'librarian-insert-trigger
        )

  )

(speckler-add! auto-modes ()
  '(yaml
    ("Procfile\\'" . yaml-mode)
    ("\\.yml\\'" . yaml-mode)
    ("\\.yaml\\'" . yaml-mode)
    ("condarc\\'" . yaml-mode)
    )
  )
(speckler-add! tree-sitter-lang ()
  '(yaml-mode . yaml)
  '(yaml-ts-mode . yaml)
  )
(speckler-add! treesit-source ()
  '(yaml "git@github.com:ikatyang/tree-sitter-yaml.git")
  )
