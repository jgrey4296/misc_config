;;; lang/jg-kotlin/+bindings.el -*- lexical-binding: t; -*-

(map! :after kotlin-mode
      :map kotlin-mode-map
      :localleader
      :desc "Docs: Android"  "1" (cmd! (browse-url "https://developer.android.com/guide"))
      :desc "Docs: Kotlin"   "2" (cmd! (browse-url "https://kotlinlang.org/docs/home.html"))
      :desc "Docs: Gradle"   "3" (cmd! (browse-url "https://docs.gradle.org/current/userguide/userguide.html"))
      :desc "Docs: Kotlin Quick Ref" "4" (cmd! (browse-url "https://kotlin-quick-reference.com/025-R-kotlin-repl.html"))
      :desc "Docs: Android Source" "5" (cmd! (find-file "/Users/johngrey/Library/Android/sdk/sources/android-30/"))

      :prefix ("b" . "build")
      :desc "gradlew assemble" "a" (cmd! (+kotlin/run-gradlew "assemble"))
      :desc "gradlew build"    "b" (cmd! (+kotlin/run-gradlew "build"))
      :desc "gradlew test"     "t" (cmd! (+kotlin/run-gradlew "test"))
)

(map! :map groovy-mode-map
      :localleader
      :desc "Docs: Gradle Reference" "1" (cmd! (browse-url "https://docs.gradle.org/current/dsl/index.html"))

      )

(map! :localleader
        :map java-mode-map
        (:prefix ("r" . "refactor")
          "gc" #'eclim-java-constructor
          "gg" #'eclim-java-generate-getter-and-setter
          "oi" #'eclim-java-import-organize
          "f"  #'eclim-java-format
          "r"  #'eclim-java-refactor-rename-symbol-at-point)
        (:prefix ("h" . "help")
          "."  #'eclim-java-show-documentation-for-current-element
          "r"  #'eclim-java-find-references
          "c"  #'eclim-java-call-hierarchy
          "h"  #'eclim-java-hierarchy
          "p"  #'eclim-problems
          "r"  #'meghanada-reference
          "t"  #'meghanada-typeinfo)
        (:prefix ("b" . "build")
          "b"  #'eclim-project-build
          "c"  #'eclim-project-create
          "d"  #'eclim-project-delete
          "g"  #'eclim-project-goto
          "i"  #'eclim-project-import
          "k"  #'eclim-project-close
          "o"  #'eclim-project-open
          "u"  #'eclim-project-update)
        )
