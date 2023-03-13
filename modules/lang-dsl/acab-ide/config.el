;; trie config.el
;; loaded fourth

(load! "+vars")
(after! jg-bindings-total
  (load! "+bindings")
  )
(after! org
  ;; TODO upgrade to org-superstar?
  (add-hook 'trie-explore-mode-hook 'org-bullets-mode)
)
(use-package! acab-ide :commands acab-ide-minor-mode)

(load! "+repl")
