;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix "b"
       :desc "Clear Popup Rules" "P" #'+jg-ui-ivy-reset-popup-rules
       )
      )

(map! :map messages-buffer-mode-map
      :after message
      :n "q" #'+popup/close
      )

(map! :map help-map
      :n "u p" #'+popup/diagnose
      )
