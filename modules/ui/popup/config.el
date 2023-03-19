;;; ui/popup/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+modes")
(load! "+macros")
(load! "+hacks")
(load! "+rules")
(load! "ivy/+ivy")
(after! (evil jg-bindings-total)
  (load! "+bindings")
  )

(add-hook 'doom-init-ui-hook   #'+popup-mode 'append)
(add-hook 'jg-ui-reapply-hook  #'+jg-ui-popup-reapply-rules)

(add-hook! '+popup-buffer-mode-hook
           #'+popup-adjust-fringes-h
           #'+popup-adjust-margins-h
           )
           ;; #'+popup-set-modeline-on-enable-h
           ;; #'+popup-unset-modeline-on-disable-h)
