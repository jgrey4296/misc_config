;;; +vars.el -*- lexical-binding: t; -*-

(defvar +java-project-package-roots (list "java/" "test/" "main/" "src/" 1)
  "A list of relative directories (strings) or depths (integer) used by
`+java-current-package' to delimit the namespace from the current buffer's full
file path. Each root is tried in sequence until one is found.

If a directory is encountered in the file path, everything before it (including
it) will be ignored when converting the path into a namespace.

An integer depth is how many directories to pop off the start of the relative
file path (relative to the project root). e.g.

Say the absolute path is ~/some/project/src/java/net/lissner/game/MyClass.java
The project root is ~/some/project
If the depth is 1, the first directory in src/java/net/lissner/game/MyClass.java
  is removed: java.net.lissner.game.
If the depth is 2, the first two directories are removed: net.lissner.game.")


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

;;-- file spec
(spec-handling-add! file-templates nil
                    ('java
                     ("/main\\.java$"    :trigger "__main"         :mode java-mode)
                     ("/src/.+\\.java$" :mode java-mode)
                     )
                    ('gradle
                     ("build\\.gradle$"           :trigger "build.gradle"    :mode groovy-mode)
                     ("settings\\.gradle$"        :trigger "settings.gradle" :mode groovy-mode)
                     )
                    ('kotlin
                     ("build\\.gradle\\.kts$"     :trigger "build.gradle.kts" :mode kotlin-mode)
                     )
                    )
;;-- end file spec

;;-- browse spec
(after! jg-ui-reapply-hook-ready
  (+jg-browse-add-lookup-spec 'java
                              '(
                                ("Kotlin" "https://kotlinlang.org/docs/home.html?q=%s&s=full")
                                ("Android" "https://developer.android.com/s/results?q=%s")
                                )
                              )
  )

;;-- end browse spec

;;-- project spec
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
(after! projectile
  (pushnew! projectile-project-root-files "build.gradle")
  (pushnew! projectile-project-root-files "build.gradle.kts")
  )

(spec-handling-add! projects nil
                    ('gradlew ("gradlew") :project-file "gradlew" :compilation-dir nil :configure nil :compile "./gradlew build" :test "./gradlew test" :install nil :package nil :run nil :test-suffix "Spec")
                    ('gradle ("build.gradle") :project-file "build.gradle" :compilation-dir nil :configure nil :compile "gradle build" :test "gradle test" :install nil :package nil :run nil :test-suffix "Spec")
                    ('jg-kotlin-project ("build.gradle" "build.gradle.kts") :project-file "build.grade")
                    )
;;-- end project spec
