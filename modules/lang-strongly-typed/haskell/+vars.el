;;; +vars.el -*- lexical-binding: t; -*-

(setq haskell-process-suggest-remove-import-lines t  ; warnings for redundant imports etc
      haskell-process-auto-import-loaded-modules t
      haskell-process-show-overlays (not (modulep! :checkers syntax))) ; redundant with flycheck

(after! smartparens-haskell
  (sp-with-modes '(haskell-mode haskell-interactive-mode)
    (sp-local-pair "{-" "-}" :actions :rem)
    (sp-local-pair "{-#" "#-}" :actions :rem)
    (sp-local-pair "{-@" "@-}" :actions :rem)
    (sp-local-pair "{-" "-")
    (sp-local-pair "{-#" "#-")
    (sp-local-pair "{-@" "@-")))


(spec-handling-add! projects
                    '(haskell-stack ("stack.yaml") :project-file "stack.yaml" :compilation-dir nil :configure nil :compile "stack build" :test "stack build --test" :install nil :package nil :run nil :test-suffix "Spec")
                    '(haskell-cabal projectile-cabal-project-p :project-file nil :compilation-dir nil :configure nil :compile "cabal build" :test "cabal test" :install nil :package nil :run "cabal run" :test-suffix "Spec")
)
(spec-handling-add! file-templates
                    '(haskell
                     (haskell-mode :trigger haskell-auto-insert-module-template :project t)
                     )
                    )
(spec-handling-add! popup
                    '(haskell
                     ("^\\*haskell\\*" :quit nil)
                     )
                    )
(spec-handling-add! lookup-url
                    '(haskell
                     ("Haskell Cabal" "https://hackage.haskell.org/packages/search?terms=%s")
                     ("Haskell Typeclassopedia" "https://wiki.haskell.org/index.php?search=%s&title=Special%3ASearch&fulltext=Search")
                     )
                    )
(spec-handling-add! lookup-handler
                    '(haskell-mode :definition haskell-mode-jump-to-def-or-tag)
                    )
(spec-handling-add! tree-sit-lang
                    '(haskell-mode . haskell)
                    )
(spec-handling-add! auto-modes
                    '(haskell
                      ("\\.[gh]s\\'" . haskell-mode)
                      ("\\.hsig\\'" . haskell-mode)
                      ("\\.l[gh]s\\'" . haskell-literate-mode)
                      ("\\.hsc\\'" . haskell-mode)
                      ("runghc" . haskell-mode)
                      ("runhaskell" . haskell-mode)
                      ("\\.cabal\\'\\|/cabal\\.project\\|/\\.cabal/config\\'" . haskell-cabal-mode)
                      ("\\.ghci\\'" . ghci-script-mode)
                      ("\\.hcr\\'" . ghc-core-mode)
                      ("\\.dump-simpl\\'" . ghc-core-mode)
                      )
                    )
(spec-handling-add! eval
                    `(haskell-mode :start ,#'+haskell/open-repl :persist t)
                    `(haskell-cabal-mode :start ,#'+haskell/open-repl :persist t)
                    `(literate-haskell-mode :start ,#'+haskell/open-repl :persist t)
                    )
