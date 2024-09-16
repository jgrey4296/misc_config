;;; lang/coq/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+proof-general")
(when (modulep! +agda) (local-load! "+agda"))
(when (modulep! +idris) (local-load! "+idris"))
(when (modulep! +fstar) (local-load! "+fstar"))
(when (modulep! +lean) (local-load! "+lean"))
(defer-load! (jg-bindings-total) "+bindings")
