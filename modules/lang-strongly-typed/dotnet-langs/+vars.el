;; -*- mode:emacs-lisp; lexical-binding: t;-*-

;;-- csharp
(push '(c-sharp "c-sharp" "tree_sitter_c_sharp") treesit-load-name-override-list)

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
                    '(csharp-mode . c-sharp)
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
                      ("\\.props\\'"      . csproj-mode)
                      ("\\.targets\\'"    . csproj-mode)
                      )
                    )
(spec-handling-add! electric
                    '(csharp-mode
                      :chars (?\n ?\})
                      )
                    )
(spec-handling-add! repl
                    '(fsharp-mode
                      :start run-fsharp
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
(spec-handling-add! popup
                    '(dotnet
                      ("^\\*.+-dis\\*" :side right  :ttl nil :width  0.4 :quit t  :select nil :priority 50)
                      )
                    )
(spec-handling-add! file-templates
                    `(csharp
                      ("Directory.Build.props\\'" :trigger "__directory_props" :mode dotnet-mode)
                      (".csproj\\'"               :trigger "__csproj"          :mode dotnet-mode)
                      (".cs\\'"                   :trigger "__cs"              :mode dotnet-mode)
                      )
                    )
;;-- end specs
