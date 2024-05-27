;;; lang/java/+meghanada.el -*- lexical-binding: t; -*-
;;;###if (modulep! +meghanada)

(use-package! meghanada
  :hook (java-mode-local-vars . meghanada-mode)
  :init
  (setq meghanada-server-install-dir (concat doom-data-dir "meghanada-server/")
        meghanada-use-company t
        meghanada-use-flycheck t
        meghanada-use-eldoc t
        meghanada-use-auto-start t)

  :config
  (spec-handling-add! lookup-handler
                      `(java-mode
                        :definition ,#'meghanada-jump-declaration
                        :references ,#'meghanada-reference
                        )
                      )

  (advice-add 'meghanada-mode :around #'+java-meghanada-fail-gracefully-a)

  (map! :localleader
        :map java-mode-map
        (:prefix ("r" . "refactor")
          "ia" #'meghanada-import-all
          "io" #'meghanada-optimize-import
          "l"  #'meghanada-local-variable
          "f"  #'meghanada-code-beautify)
        (:prefix ("h" . "help")
          "r"  #'meghanada-reference
          "t"  #'meghanada-typeinfo)
        (:prefix ("b" . "build")
          "f"  #'meghanada-compile-file
          "p"  #'meghanada-compile-project))
  )
