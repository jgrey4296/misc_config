;;; lang/jg-python/config.el -*- lexical-binding: t; -*-

(dlog! "Config JG Python")

(local-load! "+defs")
(local-load! "+vars")

(when (modulep! +builtin) (local-load! "+builtin"))
(when (modulep! +external) (local-load! "+external"))

(local-load! "+testing")
(local-load! "+extra")
(defer-load! "+envs" "+lsp" "+cython")
(defer-load! jg-bindings-total "+bindings")
