;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix "b"
       :desc "Clear Popup Rules" "P" #'+jg-ui-popup-reapply-rules
       )
      )

(map! :map messages-buffer-mode-map
      :after message
      :n "q" #'+popup/close
      )

(map! :map jg-help-map
      "u p" #'+popup/diagnose
      )

(global-set-key [remap quit-window] #'+popup/quit-window)

(map! :map helpful-mode-map
      :n "q" #'+jg-help-switch-to-prev-helpful-or-close-window
      )
