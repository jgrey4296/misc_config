;;; tools/dired/config.el -*- lexical-binding: t; -*-

(after! evil
  (load! "+bindings")
  )
(after! dired
  (load! "+vars")
  )
(load! "+funcs")
