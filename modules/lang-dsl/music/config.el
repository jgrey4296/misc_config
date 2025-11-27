;;; config.el -*- lexical-binding: t; -*-

(when (modulep! +sclang) (local-load! "+sclang"))
(when (modulep! +csound) (local-load! "+csound"))
(when (modulep! +chuck)  (local-load! "+chuck"))
(when (modulep! +tidal)  (local-load! "+tidal"))
(when (modulep! +minor) (local-load! "+minor-mode"))
(when (modulep! +faust) (local-load! "+faust"))
(local-load! "+extra")

(defer-load! jg-bindings-total "+bindings")
