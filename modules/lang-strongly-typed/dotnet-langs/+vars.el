;; -*- mode:emacs-lisp; lexical-binding: t;-*-

(speckler-add! projects ()
  '(dotnet-sln ("src") :project-file "?*.sln" :compilation-dir nil :configure nil :compile "dotnet build" :test "dotnet test" :install nil :package nil :run "dotnet run")
  '(dotnet projectile-dotnet-project-p :project-file ("?*.csproj" "?*.fsproj") :compilation-dir nil :configure nil :compile "dotnet build" :test "dotnet test" :install nil :package nil :run "dotnet run")
  )
(speckler-add! rotate-text ()
  '(csharp-mode
    :symbols (("public" "protected" "private")
              ("class" "struct"))
    )
  )
(speckler-add! doc-lookup ()
  '(fsharp-mode :async t :definition fsharp-ac/gotodefn-at-point)
  )
(speckler-add! company ()
  '(fsharp-mode (:mode fsharp-ac/company-backend))
  )
(speckler-add! auto-modes ()
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
(speckler-add! electric ()
  '(csharp-mode
    :chars (?\n ?\})
    )
  )
(speckler-add! repl ()
  '(fsharp-mode
    :start run-fsharp
    )
  )
(speckler-add! ligatures ()
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
(speckler-add! popup ()
  '(dotnet
    ("^\\*.+-dis\\*" :side right  :ttl nil :width  0.4 :quit t  :select nil :priority 50)
    )
  )
(speckler-add! file-templates ()
  `(csharp
    ("Directory.Build.props\\'" :trigger "__directory_props" :mode dotnet-mode)
    (".csproj\\'"               :trigger "__csproj"          :mode dotnet-mode)
    (".cs\\'"                   :trigger "__cs"              :mode dotnet-mode)
    )
  )
(speckler-add! babel ()
  '(dotnet
    (:name c          :lib ob-C)
    (:name fsharp     :lib ob-fsharp)
    )
  )
(speckler-add! org-src ()
  '(dotnet
    ("csharp" . csharp)
    ("fsharp" . fsharp)
    ("c#" . csharp)
    ("f#" . fsharp)
    )
  )
(speckler-add! tree-sitter-lang ()
  '(csharp-mode . c-sharp)
  )
(speckler-add! treesit-bin-override()
  '(c-sharp :lib-base "c-sharp" :entry-func "tree_sitter_c_sharp")
  )
(speckler-add! treesit-source ()
  '(csharp        "git@github.com:tree-sitter/tree-sitter-c-sharp.git")
  )
