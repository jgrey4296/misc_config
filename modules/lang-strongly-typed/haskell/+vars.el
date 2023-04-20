;;; +vars.el -*- lexical-binding: t; -*-

(setq haskell-process-suggest-remove-import-lines t  ; warnings for redundant imports etc
      haskell-process-auto-import-loaded-modules t
      haskell-process-show-overlays (not (modulep! :checkers syntax))) ; redundant with flycheck

(spec-handling-add! projects nil
                    ('haskell-stack ("stack.yaml") :project-file "stack.yaml" :compilation-dir nil :configure nil :compile "stack build" :test "stack build --test" :install nil :package nil :run nil :test-suffix "Spec")
                    ('haskell-cabal projectile-cabal-project-p :project-file nil :compilation-dir nil :configure nil :compile "cabal build" :test "cabal test" :install nil :package nil :run "cabal run" :test-suffix "Spec")
)

(spec-handling-add! lookup-handler nil
                    (haskell-mode :definition haskell-mode-jump-to-def-or-tag)
                    )

(spec-handling-add! file-templates nil
                    ('haskell
                     (haskell-mode :trigger haskell-auto-insert-module-template :project t)
                     )
                    )
(set-repl-handler!
    '(haskell-mode haskell-cabal-mode literate-haskell-mode)
    #'+haskell/open-repl :persist t)
;; Don't kill REPL popup on ESC/C-g
(spec-handling-add! popup nil
                    ('haskell
                     ("^\\*haskell\\*" :quit nil)
                     )
                    )

(spec-handling-add! lookup-regular nil
                    (haskell-mode
                     ("GHC reference" . "https://downloads.haskell.org/ghc/latest/docs/users_guide/index.html")
                     ("Typeclassopedia" . "https://wiki.haskell.org/Typeclassopedia")
                     )
                    )
