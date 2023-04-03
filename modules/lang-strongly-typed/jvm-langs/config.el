;;; lang/java/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(after! jg-bindings-total
  (load! "+bindings")

  )
(load! "+repl")
(after! projectile
  (pushnew! projectile-project-root-files "gradlew" "build.gradle"))

;;
;;; java-mode

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
  (set-docsets! 'groovy-mode "Groovy" "Groovy_JDK")
  (set-eval-handler! 'groovy-mode "groovy")
  (set-repl-handler! 'groovy-mode #'+java/open-groovy-repl))

(use-package! kotlin-mode
  :commands kotlin-mode
  :config
  (when (modulep! +lsp)
    (add-hook 'kotlin-mode-local-vars-hook #'lsp! 'append))
  (set-docsets! 'kotlin-mode "Kotlin")
  (set-repl-handler! 'kotlin-mode #'kotlin-repl)

)

(use-package! flycheck-kotlin
  :when (modulep! :checkers syntax)
  :hook (kotlin-mode . flycheck-kotlin-setup))
