;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! company
                    '(graphql-mode (:mode company-graphql))
                    )

(spec-handling-add! docsets
                    '(graphql-mode
                      "GraphQL Specification"
                      )
                    )

(spec-handling-add! electric
                    '(graphql-mode
                      :chars (?\} ?\))
                      :words ("or" "and")
                      )
                    )

(spec-handling-add! ligatures
                    '(graphql-mode
                      :null "null"
                      :true "true" :false "false"
                      :int "Int" :str "String"
                      :float "Float"
                      :bool "Bool"
                      :not "not"
                      :and "and" :or "or"
                      )
                    )
