;;; lang/graphql/config.el -*- lexical-binding: t; -*-

(use-package! graphql-mode
  :init
  ;; Define a doom-modeline compatiable major-mode icon
  (when (modulep! +lsp)
    (add-hook 'graphql-mode-local-vars-hook #'lsp! 'append)
    )
  (spec-handling-add! company
                      '(graphql-mode (:mode . #'company-graphql))
                      )

  (spec-handling-add! docsets
                      '(graphql-mode
                        "GraphQL Specification"
                        )
                      )
  (add-hook 'graphql-mode-hook #'rainbow-delimiters-mode)
  (set-electric! 'graphql-mode
    :chars '(?\} ?\))
    :words '("or" "and"))

  (set-ligatures! 'graphql-mode
    :null "null"
    :true "true" :false "false"
    :int "Int" :str "String"
    :float "Float"
    :bool "Bool"

    :not "not"
    :and "and" :or "or")
  )

(use-package! graphql-doc
  :after graphql-mode)
