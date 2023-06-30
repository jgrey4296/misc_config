;;; +vars.el -*- lexical-binding: t; -*-

(defvar +java-project-package-roots (list "java/" "test/" "main/" "src/" 1))
(defvar kotlin-repl-buffer "*kotlin*")
(defvar kotlin-command "kotlinc")

(spec-handling-add! compile-commands
                    '(jvm +jg-jvm-get-gradle-commands)
                    )


(setq scala-indent:align-parameters t
      ;; indent block comments to first asterix, not second
      scala-indent:use-javadoc-style t)

(after! projectile
  (pushnew! projectile-project-root-files "build.gradle")
  (pushnew! projectile-project-root-files "build.gradle.kts")
  )
;;-- gradle font lock
(after! groovy-mode
  (setq jg-orig-groovy-font-lock-keywords groovy-font-lock-keywords)

  (setq-default jg-groovy-font-lock-keywords
                (list
                 `(,(rx line-start (group (or "plugins"
                                              "java"
                                              "repositories"
                                              "dependencies"
                                              "sourceSets"
                                              "clean"
                                              )))
                   (1 'font-lock-doc-markup-face t)
                   )
                 `(,(rx line-start (group "task") blank (group (* word))
                        (* blank) "(" (* any) ")" blank "{" line-end)
                   (1 'font-lock-doc-markup-face t)
                   (2 'font-lock-function-name-face t)
                   )
                 `(,(rx line-start (* blank) (group (or "defaultTasks"
                                                        "version"
                                                        "group"
                                                        "id"
                                                        "mavenCentral"
                                                        "maven"
                                                        "implementation"
                                                        "main"
                                                        "java"
                                                        "resources"
                                                        "srcDir"
                                                        "description"
                                                        "doFirst"
                                                        "doLast"
                                                        "mainClass"
                                                        "args"
                                                        "classpath"
                                                        "toolchain"
                                                        "delete"
                                                        "mkdir"
                                                        "languageVersion"
                                                        "toolchain"
                                                        )))
                   (1 'font-lock-keyword-face))
                 )
                )
  (setq groovy-font-lock-keywords (append jg-groovy-font-lock-keywords
                                          jg-orig-groovy-font-lock-keywords))

  ;; (font-lock-add-keywords 'groovy-mode
  ;;                         jg-groovy-font-lock-keywords)

  )

;;-- end gradle font lock

;;-- specs
(spec-handling-add! file-templates
                    '(java
                     ("/main\\.java$"    :trigger "__main" :mode java-mode)
                     ("/src/.+\\.java$"                    :mode java-mode)
                     )
                    '(gradle
                     ("build\\.gradle$"           :trigger "build.gradle"    :mode groovy-mode)
                     ("settings\\.gradle$"        :trigger "settings.gradle" :mode groovy-mode)
                     )
                    '(kotlin
                     ("build\\.gradle\\.kts$"     :trigger "build.gradle.kts" :mode kotlin-mode)
                     )
                    )
(spec-handling-add! tree-sit-lang
                    '(java-mode       . java)
                    '(scala-mode      . scala)
                    )
(spec-handling-add! lookup-url
                    '(java
                     ("Kotlin" "https://kotlinlang.org/docs/home.html?q=%s&s=full")
                     ("Android" "https://developer.android.com/s/results?q=%s")
                     )
                    )
(spec-handling-add! projects
                    '(gradlew ("gradlew") :project-file "gradlew" :compilation-dir nil :configure nil :compile "./gradlew build" :test "./gradlew test" :install nil :package nil :run nil :test-suffix "Spec")
                    '(gradle ("build.gradle") :project-file "build.gradle" :compilation-dir nil :configure nil :compile "gradle build" :test "gradle test" :install nil :package nil :run nil :test-suffix "Spec")
                    '(jg-kotlin-project ("build.gradle" "build.gradle.kts") :project-file "build.grade")
                    )
(spec-handling-add! evil-embrace
                    '(scala-mode
                      ,(cons ?f (make-embrace-pair-struct
                                 :key ?$
                                 :left "${"
                                 :right "}"))
                      )
                    )
(spec-handling-add! auto-modes
                    '(jvm
                      ("\\.g\\(?:radle\\|roovy\\)$" . groovy-mode)
                      ("\\.kts?\\'" . kotlin-mode)
                      )
                    )
(spec-handling-add! docsets
                    '(groovy-mode "Groovy" "Groovy_JDK" "Gradle_DSL", "Gradle_Groovy_API", "Gradle_User_Guide")
                    '(kotlin-mode "Kotlin")
                      )
;;-- end specs
(spec-handling-add! lookup-regular
                    '(kotlin-mode
                     ("Android docs" . "https://developer.android.com/guide")
                     ("Kotlin reference" . "https://kotlinlang.org/docs/home.html")
                     ("Gradle Reference" . "https://docs.gradle.org/current/userguide/userguide.html")
                     ("Kotlin Quick Reference" . "https://kotlin-quick-reference.com/025-R-kotlin-repl.html")
                     ("JDK Reference" . "https://docs.oracle.com/en/java/javase/20/")
                     )
                    '(java-mode
                     ("JDK Reference" . "https://docs.oracle.com/en/java/javase/20/")
                     )
                    '(groovy-mode
                     ("Gradle Refernce" . "https://docs.gradle.org/current/dsl/index.html")
                     ("Groovy Reference" . "https://groovy-lang.org/documentation.html")
                     )
                    '(scala-mode
                     ("Scala Documentation" . "https://docs.scala-lang.org/")
                     ("Scala API" . "https://docs.scala-lang.org/api/all.html")
                     ("Scala Cheatsheet" . "https://docs.scala-lang.org/cheatsheets/index.html")
                     ("Scala Language Reference" . "https://docs.scala-lang.org/scala3/reference/")
                     ("Scala Language Spec" . "https://scala-lang.org/files/archive/spec/2.13/")
                     ("SBT Reference" . "https://www.scala-sbt.org/1.x/docs/index.html")
                     ("Scala LSP" . "https://scalameta.org/metals/docs/")
                     )
)
