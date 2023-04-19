;;; +vars.el -*- lexical-binding: t; -*-

;;-- specs
(spec-handling-add! lookup-url nil
                    ('haskell
                     ("Haskell Cabal" "https://hackage.haskell.org/packages/search?terms=%s")
                     ("Haskell Typeclassopedia" "https://wiki.haskell.org/index.php?search=%s&title=Special%3ASearch&fulltext=Search")
                     )
                    )

;;-- end specs

;; Removes haskell-mode trailing braces
(after! smartparens-haskell
  (sp-with-modes '(haskell-mode haskell-interactive-mode)
    (sp-local-pair "{-" "-}" :actions :rem)
    (sp-local-pair "{-#" "#-}" :actions :rem)
    (sp-local-pair "{-@" "@-}" :actions :rem)
    (sp-local-pair "{-" "-")
    (sp-local-pair "{-#" "#-")
    (sp-local-pair "{-@" "@-")))
