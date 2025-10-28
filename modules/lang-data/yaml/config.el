;;; lang/yaml/config.el -*- lexical-binding: t; -*-

(use-package! yaml-mode
  :commands yaml-mode
  :init
  :config
  (setq-hook! 'yaml-mode-hook tab-width yaml-indent-offset)
  (add-hook 'yaml-mode-hook #'tree-sitter!)
  (add-hook 'yaml-mode-hook #'librarian-insert-minor-mode)
  (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)

  (add-hook 'yaml-ts-mode-hook #'librarian-insert-minor-mode)
  (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode)
  (add-hook 'yaml-ts-mode-hook #'treesit-fold-mode)

  (map! :map yaml-mode-map

        )

  )

(speckler-add! auto-modes ()
  '(yaml
    ("Procfile\\'" . yaml-ts-mode)
    ("\\.yml\\'" . yaml-ts-mode)
    ("\\.yaml\\'" . yaml-ts-mode)
    ("condarc\\'" . yaml-ts-mode)
    )
  )
(speckler-add! tree-sitter-lang ()
  '(yaml-mode . yaml)
  '(yaml-ts-mode . yaml)
  )
(speckler-add! treesit-source ()
  '(yaml "git@github.com:ikatyang/tree-sitter-yaml.git")
  )
(speckler-add! fold ()
  `(yaml
    :modes yaml-mode
    :priority 25
    :triggers (:close     #'outline-indent-close-fold
               :close-all #'outline-indent-close-folds
               :open      #'outline-indent-open-fold
               :open-all  #'outline-indent-open-folds
               :open-rec  #'outline-indent-open-fold-rec
               :toggle    #'outline-indent-toggle-fold
               )
    )
  `(yaml-ts
    :modes yaml-ts-mode
    :priority 25
    :triggers (:close     #'outline-indent-close-fold
               :close-all #'outline-indent-close-folds
               :open      #'outline-indent-open-fold
               :open-all  #'outline-indent-open-folds
               :open-rec  #'outline-indent-open-fold-rec
               :toggle    #'outline-indent-toggle-fold
               )
    )
  )

(speckler-add! file-templates ()
  '(github
    ("-wf\\.yml\\'" :trigger "__github.workflow"   :mode yaml-mode :priority 100)
    )
  '(gitlab
    ("^\\.gitlab-cs\\.yml\\'" :trigger "__gitlab.root" :mode yaml-mode :priority 100)
    (".gitlab-ci\\.yml\\'"    :trigger "__gitlab.job"  :mode yaml-mode :priority 100)

    )
  )
