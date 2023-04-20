;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map haskell-mode-map
      :n "o" #'+haskell/evil-open-below
      :n "O" #'+haskell/evil-open-above
      (:when (modulep! :tools lookup)
        [remap haskell-mode-jump-to-def-or-tag] #'+lookup/definition)
      )

(map! :localleader
      :map haskell-mode-map
      "b" #'haskell-process-cabal-build
      "c" #'haskell-cabal-visit-file
      "h" #'haskell-hide-toggle
      "H" #'haskell-hide-toggle-all
      )
