;;; +vars.el -*- lexical-binding: t; -*-

;;-- browse providers
(after! jg-browse-providers
  (pushnew! jg-browse-providers-alist
            '("Haskell Cabal" "https://hackage.haskell.org/packages/search?terms=%s")
            '("Haskell Typeclassopedia" "https://wiki.haskell.org/index.php?search=%s&title=Special%3ASearch&fulltext=Search")
            )
  )

;;-- end browse providers
