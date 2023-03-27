;;; ui/popup/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+modes")
(load! "+macros")
(load! "+hacks")
(load! "+specs")
(load! "ivy/+ivy")
(after! (evil jg-bindings-total)
  (load! "+bindings")
  )

(add-hook! 'doom-init-ui-hook   #'+popup-mode 'append)
(add-hook! 'jg-ui-reapply-hook  #'+jg-popup-reapply-specs)

(add-hook! '+popup-buffer-mode-hook
           #'+popup-adjust-fringes-h
           #'+popup-adjust-margins-h
           )
           ;; #'+popup-set-modeline-on-enable-h
           ;; #'+popup-unset-modeline-on-disable-h)
