;;; lang/csharp/config.el -*- lexical-binding: t; -*-

(defer-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! csharp-mode
  :commands csharp-mode csharp-ts-mode
  :config
  (sp-local-pair 'csharp-mode "<" ">"
                 :when '(+csharp-sp-point-in-type-p)
                 :post-handlers '(("| " "SPC")))

  (add-hook! '(csharp-mode-hook csharp-ts-mode-hook)
             #'rainbow-delimiters-mode
             #'general-insert-minor-mode
             #'origami-mode
             )
  )

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
