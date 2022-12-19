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
(after! jg-completion-templates
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
(after! jg-browse-providers
  (pushnew! jg-browse-providers-alist
            '("Kotlin" "https://kotlinlang.org/docs/home.html?q=%s&s=full")
            '("Android" "https://developer.android.com/s/results?q=%s")
            )
  )

;;-- end browse providers

;;-- projectile
(after! projectile
  (pushnew! projectile-project-root-files "build.gradle")
  (pushnew! projectile-project-root-files "build.gradle.kts")

  (defun +jg-kotlin-related-files-fn (path)
    " Given a relative path to a file, provide projectile with various :kinds of related file "
    (let ((impl-file  (f-join (f-parent (f-parent path)) (s-replace "test_" "" (f-filename path))))
          (test-file  (f-join (f-parent path) "__tests" (concat "test_" (f-filename path))))
          ;;(init-file  (f-join (f-parent path) "__init__.py"))
          (log-file   (f-join (projectile-project-root) (concat "log." (f-base path))))
          ;;(error-file (f-join (car (f-split path)) "errors" (concat (f-base path) "_errors.py")))
          (project    (f-join (projectile-project-root) "project-file"))
          (is-test (s-matches? "^test_" (f-filename path)))
          )
      (append (when is-test (list :impl impl-file))
              (unless is-test (list :test test-file))
              (when (s-matches? "\/cli\/" path) (list :project project))
              (list :init-py init-file)
              (list :log log-file)
              (list :errors error-file)
              )
      )
    )

  (projectile-register-project-type 'jg-kotlin-project '("build.gradle" "build.gradle.kts")
                                    :project-file "build.grade"
                                    ;; :related-files-fn #'+jg-kotlin-related-files-fn
                                    )
  )


;;-- end projectile
