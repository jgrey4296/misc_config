;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map csharp-mode-map
      :localleader
      :desc "Docs: Unity"    "1" (cmd! (+jg-misc-browse-url "https://docs.unity3d.com/Manual/index.html"))
      :desc "Docs: Unity C#" "2" (cmd! (+jg-misc-browse-url "https://docs.unity3d.com/ScriptReference/index.html"))
      :desc "Docs: Mono"     "3" (cmd! (+jg-misc-browse-url "http://docs.go-mono.com/"))
      :desc "Docs: MS C#"    "4" (cmd! (+jg-misc-browse-url "https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/"))
)

(map! :map fsharp-mode-map
      :localleader
      :desc "Docs: F#"       "1" (cmd! (+jg-misc-browse-url "https://docs.microsoft.com/en-us/dotnet/fsharp/"))
      )

(map! :map plantuml-mode-map
      :localleader
      :desc "Docs: Plantuml" "1" (cmd! (+jg-misc-browse-url "https://plantuml.com/"))
      )

(map! :map groovy-mode-map
      :localleader
      :desc "Docs: Gradle"   "1" (cmd! (+jg-misc-browse-url "https://docs.gradle.org/current/userguide/userguide.html"))
      :desc "Docs: Groovy"   "2" (cmd! (+jg-misc-browse-url "https://groovy-lang.org/documentation.html"))
      )