;; trie config.el
;; loaded fourth

(load! "+vars")
(after! evil
  (load! "+bindings")
  )
(after! org
  ;; TODO upgrade to org-superstar?
  (add-hook 'trie-mode-hook 'org-bullets-mode)
)
(after! helm
  (load! "+helms")
  )

(use-package! acab-ide
  :commands acab-ide-minor-mode
  )
