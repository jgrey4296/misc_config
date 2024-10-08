;;; lang/java/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+extra-config")
(defer-load! jg-bindings-total "+bindings")
(after! ivy
  (ivy-configure 'jg-vcs-gradle-ivy
    :format-fn #'jg-vcs-format-gradle)
)

(use-package! java-mode
  :commands java-mode
  :config
  (add-hook 'java-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'java-mode-local-vars-hook #'tree-sitter! 'append)
  )

(use-package! android-mode
  :commands android-mode
  :init
  (add-hook! '(java-mode-hook groovy-mode-hook nxml-mode-hook)
             #'+java-android-mode-maybe-h)
  )

(use-package! groovy-mode
  :commands groovy-mode
  )

(use-package! kotlin-mode
  :commands kotlin-mode
)

(use-package! kotlin-ts-mode
  :defer t
  )

(use-package! scala-mode
  :commands scala-mode
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
