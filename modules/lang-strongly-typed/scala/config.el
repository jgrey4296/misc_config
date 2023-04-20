;;; lang/scala/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(after! jg-bindings-total
  (load! "+bindings"))
(after! projectile
  (add-to-list 'projectile-project-root-files "build.sbt"))

;;
;;; Packages

(use-package! scala-mode
  :defer t
  :config

  (setq-hook! 'scala-mode-hook
    comment-line-break-function #'+scala-comment-indent-new-line-fn)

  (when (modulep! +lsp)
    (setq-hook! 'scala-mode-hook lsp-enable-indentation nil)
    (add-hook 'scala-mode-local-vars-hook #'lsp! 'append))

  (when (modulep! +tree-sitter)
    (add-hook 'scala-mode-local-vars-hook #'tree-sitter! 'append))

  (set-ligatures! 'scala-mode
    ;; Functional
    :def "def"
    :composition  "compose"
    ;; HKT
    :lambda       "Lambda"
    ;; Types
    :null         "none"
    :null         "None"
    :true         "true"
    :false        "false"
    :int          "Int"
    :str          "String"
    :float        "Float"
    :bool         "Boolean"
    :list         "List"
    ;; Flow
    :for          "for"
    :not          "!"
    :and          "&&"
    :or           "||"
    :yield        "yield"
    ;; Other
    :union        "union"
    :intersect    "intersect"
    :diff         "diff")
  )

(use-package! sbt-mode
  :after scala-mode
  :config
  (set-repl-handler! 'scala-mode #'+scala/open-repl :persist t)
  )
