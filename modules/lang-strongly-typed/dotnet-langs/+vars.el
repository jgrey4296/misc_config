

;;-- csharp

;;-- end csharp

;;-- fsharp

;;-- end fsharp

;;-- unity

;;-- end unity

;;-- specs
(spec-handling-add! projects nil
                    ('dotnet-sln ("src") :project-file "?*.sln" :compilation-dir nil :configure nil :compile "dotnet build" :test "dotnet test" :install nil :package nil :run "dotnet run")
                    ('dotnet #'projectile-dotnet-project-p :project-file ("?*.csproj" "?*.fsproj") :compilation-dir nil :configure nil :compile "dotnet build" :test "dotnet test" :install nil :package nil :run "dotnet run")
                    )
(spec-handling-add! lookup-regular nil
                    (csharp-mode
                     ("Unity Manual" . "https://docs.unity3d.com/Manual/index.html")
                     ("Unity C# Reference" . "https://docs.unity3d.com/ScriptReference/index.html")
                     ("Mono Reference" . "http://docs.go-mono.com/")
                     ("Microsoft C# reference" . "https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/")

                     )
                    (fsharp-mode
                     ("Microsoft F# reference" . "https://docs.microsoft.com/en-us/dotnet/fsharp/")
                     )
                    )
;;-- end specs
