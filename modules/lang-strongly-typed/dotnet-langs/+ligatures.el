;;; +ligatures.el -*- lexical-binding: t; -*-

(set-ligatures! 'csharp-mode
  ;; Functional
  :lambda        "() =>"
  ;; Types
  :null          "null"
  :true          "true"
  :false         "false"
  :int           "int"
  :float         "float"
  :str           "string"
  :bool          "bool"
  :list          "List"
  ;; Flow
  :not           "!"
  :in            "in"
  :and           "&&"
  :or            "||"
  :for           "for"
  :return        "return"
  :yield         "yield")
