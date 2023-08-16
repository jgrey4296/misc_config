;; trie config.el
;; loaded fourth

(defer-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(after! org
  ;; TODO upgrade to org-superstar?
  (add-hook 'trie-explore-mode-hook 'org-bullets-mode)
)
(use-package! acab-ide :commands acab-ide-minor-mode)
