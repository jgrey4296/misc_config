;;; +vars.el -*- lexical-binding: t; -*-

(defvar +java-project-package-roots (list "java/" "test/" "main/" "src/" 1))

(defvar kotlin-repl-buffer "*kotlin*")

(defvar kotlin-command "kotlinc")

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
                      ("\\.kts?\\'"                 . kotlin-mode)
                      ("\\.java\\'"                 . java-mode)
                      ("\\.scala\\'"                . scala-mode)
                      ("\\.sc\\'"                   . scala-mode)
                      )
                    )
(spec-handling-add! docsets
                    '(groovy-mode "Groovy" "Groovy_JDK" "Gradle_DSL", "Gradle_Groovy_API", "Gradle_User_Guide")
                    '(kotlin-mode "Kotlin")
                      )
(spec-handling-add! eval
                    `(kotlin-mode :start ,#'+kotlin-mode/open-repl)
                    `(kotlin-mode :start ,#'kotlin-repl :name kotlin-alt)
                    `(groovy-mode :start ,#'+java/open-groovy-repl)
                    `(scala-mode :start ,#'+scala/open-repl :persist t)
                    )
(spec-handling-add! ligatures
                    '(scala-mode
                      ;; Functional
                      :def "def"
                      :composition  "compose"
                      ;; HKT
                      :lambda       "Lambda"
                      ;; Types
                      :null         "none"
                      :null         "None"
                      :true         "true"
                      :false        "false"
                      :int          "Int"
                      :str          "String"
                      :float        "Float"
                      :bool         "Boolean"
                      :list         "List"
                      ;; Flow
                      :for          "for"
                      :not          "!"
                      :and          "&&"
                      :or           "||"
                      :yield        "yield"
                      ;; Other
                      :union        "union"
                      :intersect    "intersect"
                      :diff         "diff"
                      )
                    )
(spec-handling-add! yas-extra
                    '(android-mode android-mode)
                    )
(spec-handling-add! compile-commands
                    '(jvm +jg-jvm-get-gradle-commands)
                    )
;;-- end specs
