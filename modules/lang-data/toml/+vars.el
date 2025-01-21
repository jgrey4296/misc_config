;;; +vars.el -*- lexical-binding: t; -*-

(after! projectile
  (pushnew! projectile-project-root-files "doot.toml")
  )

(speckler-add! imenu ()
  :override t
  '(toml-mode
    ("Multi-Group" "^\\[\\[\\(.+?\\)\\]\\]$" 1)
    ("Group" "^\\[\\([^\\[]+?\\)\\]$" 1)
    )
  )
(speckler-add! whitespace-cleanup ()
  `(conf-toml-mode
    ,#'+jg-toml-cleanup-ensure-newline-before-table
    ,#'delete-trailing-whitespace
    ,#'+jg-text-cleanup-whitespace
    )
  `(toml-mode
    ,#'+jg-toml-cleanup-ensure-newline-before-table
    ,#'delete-trailing-whitespace
    ,#'+jg-text-cleanup-whitespace
    )
  )
(speckler-add! auto-modes ()
  '(toml
    ("\\.toml\\'" . toml-mode)
    )
  '(conf
    ("\\.conf\\'" . conf-mode)
    ("\\.cnf\\'" . conf-mode)
    ("\\.cfg\\'" . conf-mode)
    )
  )
(speckler-add! fold ()
  `(toml
    :modes (conf-toml-mode toml-mode toml-ts-mode)
    :priority -50
    :triggers (:open-all   #'outline-show-all
               :close-all  (cmd! (with-no-warnings (outline-hide-sublevels 1)))
               :toggle     #'outline-toggle-children
               :open       (cmd! (with-no-warnings (outline-show-entry) (outline-show-children)))
               :open-rec   #'outline-show-subtree
               :close      #'outline-hide-subtree
               )
    )
  )
(speckler-add! org-src ()
  '(toml
    ("toml" . toml)
    )
  )
(speckler-add! treesit-source ()
  '(toml          "git@github.com:ikatyang/tree-sitter-toml.git")
  )
(speckler-add! treesit-lang ()
  '(toml-mode . toml)
  '(toml-ts-mode . toml)
  '(toml-conf-mode . toml)
)
(speckler-add! file-templates ()
  '(doot
    (".+?\\.tsk\\.toml\\'" :mode toml-mode :trigger "__tasks" :priority 100)
    )
)
