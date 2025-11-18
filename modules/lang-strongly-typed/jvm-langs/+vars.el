;;; +vars.el -*- lexical-binding: t; -*-

(defvar +java-project-package-roots (list "java/" "test/" "main/" "src/" 1))

(defvar kotlin-repl-buffer "*repl*")

(defvar kotlin-command "ki")
(defvar kotlin-args-rep nil)

(setq scala-indent:align-parameters t
      ;; indent block comments to first asterix, not second
      scala-indent:use-javadoc-style t)

(after! projectile
  (pushnew! projectile-project-root-files "build.gradle")
  (pushnew! projectile-project-root-files "build.gradle.kts")
  )

;;-- specs
(speckler-add! file-templates ()
  '(java
    ("/main\\.java$"    :trigger "__main" :mode java-mode)
    ("/src/.+\\.java$"                    :mode java-mode)
    )
  '(kotlin
    ("build\\.gradle\\.kts$"     :trigger "build.gradle.kts" :mode kotlin-mode)
    )
  )
(speckler-add! tree-sitter-lang ()
  '(java-mode          . java)
  '(scala-mode         . scala)
  '(kotlin-mode        . kotlin)
  '(kotlin-ts-mode     . kotlin)
  )
(speckler-add! treesit-source ()
  '(java   "git@github.com:tree-sitter/tree-sitter-java.git")
  '(kotlin "git@github.com:tree-sitter/kotlin-tree-sitter")
  )
(speckler-add! online-search ()
  '(java
    ("Kotlin" "https://kotlinlang.org/docs/home.html?q=%s&s=full")
    ("Android" "https://developer.android.com/s/results?q=%s")
    )
  )
(speckler-add! projects ()
  '(gradle ("build.gradle.kts") :project-file "build.gradle..kts" :compilation-dir nil :configure nil :compile "gradle build" :test "gradle test" :install nil :package nil :run nil :test-suffix "Spec")
  '(jg-kotlin-project ("build.gradle" "build.gradle.kts") :project-file "build.grade")
  )
(speckler-add! evil-embrace ()
  '(scala-mode
    ,(cons ?f (make-embrace-pair-struct
               :key ?$
               :left "${"
               :right "}"))
    )
  )
(speckler-add! auto-modes ()
  '(jvm
    ("\\.kt\\'"                     . kotlin-ts-mode)
    ("\\.kts?\\'"                   . kotlin-ts-mode)
    ("\\.java\\'"                   . java-mode)
    ("\\.scala\\'"                  . scala-mode)
    ("\\.sc\\'"                     . scala-mode)
    )
  )
(speckler-add! docsets ()
  '(kotlin-mode "Kotlin")
  )
(speckler-add! repl ()
  '(kotlin-mode :start +kotlin-mode/open-repl)
  '(scala-mode  :start +scala/open-repl :persist t)
  )
(speckler-add! ligatures ()
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
(speckler-add! yas-extra ()
  '(android-mode android-mode)
  )
(speckler-add! compile-commands ()
  '(jvm #'+jg-jvm-get-gradle-commands)
  )
(speckler-add! babel ()
  '(jvm
    (:name groovy     :lib ob-groovy)
    (:name java       :lib ob-java)
    )
  )
(speckler-add! org-src ()
  '(jvm
    ("java" . java)
    ("groovy" . groovy)
    ("kotlin" . kotlin)
    )
  )
(speckler-add! fold ()
  :override t
  `(kotlin
    :modes (kotlin-ts-mode)
    :priority 25
    :triggers (:close     #'treesit-fold-close
               :close-all #'treesit-fold-close-all
               :open      #'treesit-fold-open
               :open-all  #'treesit-fold-open-all
               :open-rec  #'treesit-fold-open-recursively
               :toggle    #'treesit-fold-toggle
               )
    )
  )
;;-- end specs
