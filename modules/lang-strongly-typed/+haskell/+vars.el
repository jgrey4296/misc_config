;;; +vars.el -*- lexical-binding: t; -*-

;;-- browse spec
(after! jg-ui-reapply-hook-ready
   (+jg-browse-add-lookup-spec 'haskell
                               '(
                                ("Haskell Cabal" "https://hackage.haskell.org/packages/search?terms=%s")
                                ("Haskell Typeclassopedia" "https://wiki.haskell.org/index.php?search=%s&title=Special%3ASearch&fulltext=Search")
            )
  )
   )

;;-- end browse spec

;; Removes haskell-mode trailing braces
(after! smartparens-haskell
  (sp-with-modes '(haskell-mode haskell-interactive-mode)
    (sp-local-pair "{-" "-}" :actions :rem)
    (sp-local-pair "{-#" "#-}" :actions :rem)
    (sp-local-pair "{-@" "@-}" :actions :rem)
    (sp-local-pair "{-" "-")
    (sp-local-pair "{-#" "#-")
    (sp-local-pair "{-@" "@-")))
