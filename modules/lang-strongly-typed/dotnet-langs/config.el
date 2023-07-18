;;; lang/csharp/config.el -*- lexical-binding: t; -*-

(defer-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! csharp-mode
  :commands csharp-mode
  :hook (csharp-mode . rainbow-delimiters-mode)
  :config

  (sp-local-pair 'csharp-mode "<" ">"
                 :when '(+csharp-sp-point-in-type-p)
                 :post-handlers '(("| " "SPC")))

  (when (modulep! +lsp)
    (add-hook 'csharp-mode-local-vars-hook #'lsp! 'append))

  (defadvice! +csharp-disable-clear-string-fences-a (fn &rest args)
    "This turns off `c-clear-string-fences' for `csharp-mode'. When
on for `csharp-mode' font lock breaks after an interpolated string
or terminating simple string."
    :around #'csharp-disable-clear-string-fences
    (unless (eq major-mode 'csharp-mode)
      (apply fn args))))

(use-package! csharp-tree-sitter
  :after csharp-mode
  :init
  (add-hook 'csharp-mode-local-vars-hook #'tree-sitter! 'append)
  (if (fboundp #'csharp-tree-sitter-mode)
      (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))))

(use-package! fsharp-mode
  :commands fsharp-mode
  :config
  (when (executable-find "dotnet")
    (setq inferior-fsharp-program "dotnet fsi --readline-"))

  (setq fsharp-ac-intellisense-enabled nil
        fsharp-ac-use-popup nil ; Use a buffer for docs rather than a pop-up
    )
  )

;; Unity shaders
(use-package! shader-mode
  :after csharp-mode
  :config
  (def-project-mode! +csharp-unity-mode
    :modes '(csharp-mode shader-mode)
    :files (and "Assets" "Library/MonoManager.asset" "Library/ScriptMapper")))

(use-package! sharper
  :after chsarp-mode
  :general ("C-c d" #'sharper-main-transient)
  :config
  (map! (:map sharper--solution-management-mode-map
         :nv "RET" #'sharper-transient-solution
         :nv "gr" #'sharper--solution-management-refresh)
        (:map sharper--project-references-mode-map
         :nv "RET" #'sharper-transient-project-references
         :nv "gr" #'sharper--project-references-refresh)
        (:map sharper--project-packages-mode-map
         :nv "RET" #'sharper-transient-project-packages
         :nv "gr" #'sharper--project-packages-refresh)
        (:map sharper--nuget-results-mode-map
         :nv "RET" #'sharper--nuget-search-install))
  )

(use-package! sln-mode
  :after csharp-mode
  )
