;;; lang/java/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(defer-load! jg-bindings-total "+bindings")
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

  (add-hook 'java-mode-local-vars-hook #'tree-sitter! 'append)
  )

(use-package! android-mode
  :commands android-mode
  :init
  (add-hook! '(java-mode-hook groovy-mode-hook nxml-mode-hook)
             #'+java-android-mode-maybe-h)
  )

(use-package! groovy-mode :defer t)

(use-package! kotlin-mode
  :commands kotlin-mode
)

(use-package! flycheck-kotlin
  :when (modulep! :checkers syntax)
  :hook (kotlin-mode . flycheck-kotlin-setup)
  )

(use-package! scala-mode
  :defer t
  :config
  (setq-hook! 'scala-mode-hook
    comment-line-break-function #'+scala-comment-indent-new-line-fn
    lsp-enable-indentation nil)

  (add-hook 'scala-mode-local-vars-hook #'tree-sitter!)

  )

(use-package! sbt-mode
  :after scala-mode
  :config
  (after! projectile
    (add-to-list 'projectile-project-root-files "build.sbt"))
  )
