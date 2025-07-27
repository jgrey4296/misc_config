;;; tools/debugger/config.el -*- lexical-binding: t; -*-

(when (modulep! +gud)     (local-load! "+gud"))
(when (modulep! +realgud) (local-load! "+realgud"))
(when (modulep! +dap)     (local-load! "+dap"))
(when (modulep! +edebug)  (local-load! "+edebug"))

(defer-load! jg-total-bindings "+bindings")
