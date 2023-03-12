;;; lang/java/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(after! jg-bindings-total
  (load! "+bindings")

  )
(load! "+repl")
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

(after! projectile
  (pushnew! projectile-project-root-files "gradlew" "build.gradle"))


;;
;;; java-mode

(add-hook 'java-mode-hook #'rainbow-delimiters-mode)

(cond ((modulep! +meghanada) (load! "+meghanada"))
      ((modulep! :tools lsp +eglot))
      ((modulep! +lsp)       (load! "+lsp")))

(when (modulep! +tree-sitter)
  (add-hook 'java-mode-local-vars-hook #'tree-sitter! 'append))


;;
;;; Common packages

(use-package! android-mode
  :commands android-mode
  :init
  (add-hook! '(java-mode-hook groovy-mode-hook nxml-mode-hook)
             #'+java-android-mode-maybe-h)
  :config
  (set-yas-minor-mode! 'android-mode))


(use-package! groovy-mode
  :mode "\\.g\\(?:radle\\|roovy\\)$"
  :config
  (set-docsets! 'groovy-mode "Groovy" "Groovy_JDK")
  (set-eval-handler! 'groovy-mode "groovy")
  (set-repl-handler! 'groovy-mode #'+java/open-groovy-repl))

(after! kotlin-mode
  (when (modulep! +lsp)
    (add-hook 'kotlin-mode-local-vars-hook #'lsp! 'append))
  (set-docsets! 'kotlin-mode "Kotlin")
  (set-repl-handler! 'kotlin-mode #'kotlin-repl)

  (map! :map kotlin-mode-map
        :localleader
        :prefix ("b" . "build")
        :desc "gradlew assemble" "a" (cmd! (+kotlin/run-gradlew "assemble"))
        :desc "gradlew build"    "b" (cmd! (+kotlin/run-gradlew "build"))
        :desc "gradlew test"     "t" (cmd! (+kotlin/run-gradlew "test"))))


(use-package! flycheck-kotlin
  :when (modulep! :checkers syntax)
  :hook (kotlin-mode . flycheck-kotlin-setup))
