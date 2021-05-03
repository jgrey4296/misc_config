(load! "+vars")

(after! evil
  (load! "+bindings")
  (add-hook! doom-first-input
             #'+jg-completion-binding-hook)
)
