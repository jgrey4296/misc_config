;;; lang/jg-kotlin/+bindings.el -*- lexical-binding: t; -*-

(map! :map kotlin-mode-map
      :localleader
      :desc "Docs: Android"  "1" (cmd! (+jg-browse-url "https://developer.android.com/docs"))
      :desc "Docs: Kotlin"   "2" (cmd! (+jg-browse-url "https://kotlinlang.org/docs/home.html"))
      :desc "Docs: Gradle"   "3" (cmd! (+jg-browse-url "https://docs.gradle.org/current/userguide/userguide.html"))
)
