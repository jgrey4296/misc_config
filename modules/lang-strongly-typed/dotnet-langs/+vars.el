;; -*- mode:emacs-lisp; lexical-binding: t;-*-

;;-- csharp

;;-- end csharp

;;-- fsharp

;;-- end fsharp

;;-- unity

;;-- end unity

;;-- specs
(spec-handling-add! projects
                    '(dotnet-sln ("src") :project-file "?*.sln" :compilation-dir nil :configure nil :compile "dotnet build" :test "dotnet test" :install nil :package nil :run "dotnet run")
                    '(dotnet projectile-dotnet-project-p :project-file ("?*.csproj" "?*.fsproj") :compilation-dir nil :configure nil :compile "dotnet build" :test "dotnet test" :install nil :package nil :run "dotnet run")
                    )
(spec-handling-add! rotate-text
                    '(csharp-mode
                     :symbols (("public" "protected" "private")
                               ("class" "struct"))
                     )
                    )
(spec-handling-add! tree-sit-lang
                    '(csharp-mode     . c-sharp)
                    )
(spec-handling-add! lookup-handler
                    '(fsharp-mode :async t :definition fsharp-ac/gotodefn-at-point)
                    )
(spec-handling-add! company
                    '(fsharp-mode (:mode fsharp-ac/company-backend))
                    )
(spec-handling-add! auto-modes
                    '(dotnet
                      ("\\.shader\\'"     . shader-mode)
                      ( "\\.sln\\'"       . sln-mode)
                      ("\\.cs\\'"         . csharp-mode)
                      ("\\.fs[iylx]?\\'"  . fsharp-mode)
                      ("\\.[^.]*proj\\'"  . csproj-mode)
                      )
                    )
(spec-handling-add! electric
                    '(csharp-mode
                      :chars (?\n ?\})
                      )
                    )
(spec-handling-add! eval
                    `(fsharp-mode
                      :start ,#'run-fsharp
                      )
                    )
(spec-handling-add! ligatures
                    '(csharp-mode
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
                      :yield         "yield"
                      )
                    )
;;-- end specs
