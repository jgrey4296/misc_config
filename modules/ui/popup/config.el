;;; ui/popup/config.el -*- lexical-binding: t; -*-

(local-load! "+defs")
(local-load! "+vars")
(local-load! "+modes")
(local-load! "+hacks")
(local-load! "+spec-defs")
(local-load! "+specs")
(defer-load! jg-bindings-total "+bindings")

(add-hook! 'doom-init-ui-hook :append #'+popup-mode)

(add-hook! '+popup-buffer-mode-hook
           #'+popup-adjust-fringes-h
           #'+popup-adjust-margins-h
           ;; #'+popup-set-modeline-on-enable-h
           ;; #'+popup-unset-modeline-on-disable-h
           )
