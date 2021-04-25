(load! "+bindings")
(load! "+vars")

(add-hook! doom-first-input
           #'+jg-completion-binding-hook)
