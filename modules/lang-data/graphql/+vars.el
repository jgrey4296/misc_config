;;; +vars.el -*- lexical-binding: t; -*-

(speckler-add! company ()
  '(graphql-mode (:mode company-graphql))
  )

(speckler-add! docsets ()
  '(graphql-mode
    "GraphQL Specification"
    )
  )

(speckler-add! electric ()
  '(graphql-mode
    :chars (?\} ?\))
    :words ("or" "and")
    )
  )

(speckler-add! ligatures ()
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
