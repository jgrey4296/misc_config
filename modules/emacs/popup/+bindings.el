;;; +bindings.el -*- lexical-binding: t; -*-


(map! :leader
      :desc "Popup Buffer"          "<"     #'+jg-popup-ivy-buffer
      )

(map! :map messages-buffer-mode-map
      :after message
      :n "q" #'+popup/close
      )

(map! :map jg-help-map
      "u p" #'+popup/diagnose
      )

(global-set-key [remap quit-window] #'+popup/quit-window)
