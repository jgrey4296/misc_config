;;; lang/jg-kotlin/+bindings.el -*- lexical-binding: t; -*-

(map! :after kotlin-mode
      :map kotlin-mode-map
      :localleader
      :desc "Docs: Android Source" "5" (cmd! (find-file "/Users/johngrey/Library/Android/sdk/sources/android-30/"))

      :prefix ("b" . "build")
      :desc "gradlew assemble" "a" (cmd! (+kotlin/run-gradlew "assemble"))
      :desc "gradlew build"    "b" (cmd! (+kotlin/run-gradlew "build"))
      :desc "gradlew test"     "t" (cmd! (+kotlin/run-gradlew "test"))
      )

(map! :map groovy-mode-map
      :localleader

      )

(map! :map java-mode-map
      :localleader
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

(map! :leader
      :prefix "p m"
      :desc "Gradle -q" :n "g" #'+jg-vcs-run-gradle-quiet
      :desc "Gradle"    :n "G" #'+jg-vcs-run-gradle
      )
