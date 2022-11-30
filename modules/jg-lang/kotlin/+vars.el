;;; +vars.el -*- lexical-binding: t; -*-

(setq-default kotlin-repl-buffer "*kotlin*"
              kotlin-command "kotlinc")

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

;;-- file templates
(after! jg-file-templates
  ;; groovy-mode
  (+jg-completion-add-file-templates
   'java-and-friends
   '(
     ;; java
     ("/main\\.java$"    :trigger "__main"         :mode java-mode)
     ("/src/.+\\.java$" :mode java-mode)

     ;; Gradle
     ("build\\.gradle$"           :trigger "build.gradle"    :mode groovy-mode)
     ("settings\\.gradle$"        :trigger "settings.gradle" :mode groovy-mode)

     ;; Kotlin
     ("build\\.gradle\\.kts$"     :trigger "build.gradle.kts" :mode kotlin-mode)
     )
   )
  )
;;-- end file templates

;;-- browse providers
(after! 'jg-browse-providers
  (pushnew! jg-browse-providers-alist
            '("Kotlin" "https://kotlinlang.org/docs/home.html?q=%s&s=full")
            '("Android" "https://developer.android.com/s/results?q=%s")
            )
  )

;;-- end browse providers
