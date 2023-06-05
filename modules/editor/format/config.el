;;; editor/format/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(add-to-list 'doom-debug-variables 'format-all-debug)
(when (modulep! +onsave)
  (add-hook 'after-change-major-mode-hook #'+format-enable-on-save-maybe-h))
