;;; lang/csharp/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(after! (evil jg-bindings-total)
  (load! "+bindings")
  )

(use-package! csharp-mode
  :defer t
  :hook (csharp-mode . rainbow-delimiters-mode)
  :config
  (set-electric! 'csharp-mode :chars '(?\n ?\}))
  (spec-handling-add! rotate-text nil
                      (csharp-mode
                       :symbols '(("public" "protected" "private")
                                  ("class" "struct"))
                       )
                      )

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
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (add-hook 'csharp-mode-local-vars-hook #'tree-sitter! 'append)
  (if (fboundp #'csharp-tree-sitter-mode)
      (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))))

(use-package! fsharp-mode
  :defer t
  :config
  (when (executable-find "dotnet")
    (setq inferior-fsharp-program "dotnet fsi --readline-"))

  (if (modulep! +lsp)
      (progn
        (setq fsharp-ac-intellisense-enabled nil)
        (add-hook 'fsharp-mode-local-vars-hook #'lsp! 'append))

    (setq fsharp-ac-use-popup nil) ; Use a buffer for docs rather than a pop-up
    (spec-handling-add! lookup-handler nil (fsharp-mode :async t :definition #'fsharp-ac/gotodefn-at-point))
    (spec-handling-add! company nil (fsharp-mode fsharp-ac/company-backend)))
  (set-repl-handler! 'fsharp-mode #'run-fsharp)
  )

;; Unity shaders
(use-package! shader-mode
  :when (modulep! +unity)
  :mode "\\.shader\\'"
  :config
  (def-project-mode! +csharp-unity-mode
    :modes '(csharp-mode shader-mode)
    :files (and "Assets" "Library/MonoManager.asset" "Library/ScriptMapper")))

(use-package! sharper
  :when (modulep! +dotnet)
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
         :nv "RET" #'sharper--nuget-search-install)))

(use-package! sln-mode :mode "\\.sln\\'")
