;; trie packages.el
;; loads second

(defconst jg-trie-layer-packages
  '(
    (trie-face :location local)
    (trie-tree :location local)
    (trie-mode :location local)
    (trie-sequence-mode :location local)
    (trie-explore-mode :location local)
    (trie-minor-mode :location local)
    (parsec :location elpa :step pre)
    (font-lock+ :location (recipe :fetcher git :url "https://github.com/emacsmirror/font-lock-plus"))
    helm
    )
  )

