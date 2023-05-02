;;; lang/java/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(after! jg-bindings-total
  (load! "+bindings")

  )
(load! "+repl")
(after! projectile
  (pushnew! projectile-project-root-files "gradlew" "build.gradle"))
(after! ivy
  (ivy-configure 'jg-vcs-gradle-ivy
    :format-fn #'jg-vcs-format-gradle)
)

(use-package! java-mode
  :commands java-mode
  :config
  (add-hook 'java-mode-hook #'rainbow-delimiters-mode)

  (cond ((modulep! +meghanada) (load! "+meghanada"))
        ((modulep! :tools lsp +eglot))
        ((modulep! +lsp)       (load! "+lsp")))

  (when (modulep! +tree-sitter)
    (add-hook 'java-mode-local-vars-hook #'tree-sitter! 'append))
  )

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
  (spec-handling-add! docsets '(groovy-mode "Groovy" "Groovy_JDK"))
  (set-eval-handler! 'groovy-mode "groovy")
  (set-repl-handler! 'groovy-mode #'+java/open-groovy-repl)
  )

(use-package! kotlin-mode
  :commands kotlin-mode
  :config
  (when (modulep! +lsp)
    (add-hook 'kotlin-mode-local-vars-hook #'lsp! 'append))
  (spec-handling-add! docsets '(kotlin-mode "Kotlin"))
  (set-repl-handler! 'kotlin-mode #'kotlin-repl)

)

(use-package! flycheck-kotlin
  :when (modulep! :checkers syntax)
  :hook (kotlin-mode . flycheck-kotlin-setup))

(use-package! scala-mode
  :defer t
  :config

  (setq-hook! 'scala-mode-hook
    comment-line-break-function #'+scala-comment-indent-new-line-fn)

  (when (modulep! +lsp)
    (setq-hook! 'scala-mode-hook lsp-enable-indentation nil)
    (add-hook 'scala-mode-local-vars-hook #'lsp! 'append))

  (when (modulep! +tree-sitter)
    (add-hook 'scala-mode-local-vars-hook #'tree-sitter! 'append))

  (set-ligatures! 'scala-mode
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
    :diff         "diff")
  )

(use-package! sbt-mode
  :after scala-mode
  :config
  (set-repl-handler! 'scala-mode #'+scala/open-repl :persist t)
  (after! projectile
    (add-to-list 'projectile-project-root-files "build.sbt"))
  )
