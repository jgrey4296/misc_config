
(map! :map csharp-mode-map
      :localleader
      :desc "Docs: Unity"    "1" (cmd! (browse-url "https://docs.unity3d.com/Manual/index.html"))
      :desc "Docs: Unity C#" "2" (cmd! (browse-url "https://docs.unity3d.com/ScriptReference/index.html"))
      :desc "Docs: Mono"     "3" (cmd! (browse-url "http://docs.go-mono.com/"))
      :desc "Docs: MS C#"    "4" (cmd! (browse-url "https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/"))
)

(map! :map fsharp-mode-map
      :localleader
      :desc "Docs: F#"       "1" (cmd! (browse-url "https://docs.microsoft.com/en-us/dotnet/fsharp/"))
      )
