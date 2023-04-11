

;;-- csharp

;;-- end csharp

;;-- fsharp

;;-- end fsharp

;;-- unity

;;-- end unity

;;-- project specs
(after! jg-ui-reapply-hook-ready
  (+jg-projects-add-spec 'dotnet-sln '(("src")                       :project-file "?*.sln"                  :compilation-dir nil :configure nil :compile "dotnet build"               :test "dotnet test"                            :install nil :package nil             :run "dotnet run"))
  (+jg-projects-add-spec 'dotnet '(projectile-dotnet-project-p       :project-file ("?*.csproj" "?*.fsproj") :compilation-dir nil :configure nil :compile "dotnet build"               :test "dotnet test"                            :install nil :package nil             :run "dotnet run"))
  )
;;-- end project specs
