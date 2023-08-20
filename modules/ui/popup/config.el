;;; ui/popup/config.el -*- lexical-binding: t; -*-

(load! "+defs")
(load! "+vars")
(load! "+modes")
(load! "+macros")
(load! "+hacks")
(load! "+spec-defs")
(load! "+specs")
(defer-load! ivy "ivy/+ivy")
(defer-load! jg-bindings-total "+bindings")

(add-hook! 'doom-init-ui-hook :append #'+popup-mode)

(add-hook! '+popup-buffer-mode-hook
           #'+popup-adjust-fringes-h
           #'+popup-adjust-margins-h
           ;; #'+popup-set-modeline-on-enable-h
           ;; #'+popup-unset-modeline-on-disable-h
           )
