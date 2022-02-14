;;; lang/jg-kotlin/+bindings.el -*- lexical-binding: t; -*-

(map! :map kotlin-mode-map
      :localleader
      :desc "Docs: Android"  "1" (cmd! (+jg-browse-url "https://developer.android.com/guide"))
      :desc "Docs: Kotlin"   "2" (cmd! (+jg-browse-url "https://kotlinlang.org/docs/home.html"))
      :desc "Docs: Gradle"   "3" (cmd! (+jg-browse-url "https://docs.gradle.org/current/userguide/userguide.html"))
      :desc "Docs: Kotlin Quick Ref" "4" (cmd! (+jg-browse-url "https://kotlin-quick-reference.com/025-R-kotlin-repl.html"))
      :desc "Docs: Android Source" "5" (cmd! (find-file "/Users/johngrey/Library/Android/sdk/sources/android-30/"))
)
