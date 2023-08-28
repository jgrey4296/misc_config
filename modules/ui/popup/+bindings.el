;;; +bindings.el -*- lexical-binding: t; -*-


(map! :leader
      :desc "Popup Buffer"          "<"     #'+jg-popup-ivy-buffer
      )

(map! :map +popup-buffer-mode-map
      :n "q" #'+popup/close
      )

(map! :map jg-help-map
      "u ?" #'+popup/diagnose
      )

(map! :leader
      :desc "SCRATCH"                      "6" (cmd! (+jg-popup-ivy-open "*scratch*"))
      :desc "Messages"                     "0" #'+jg-ivy-popup-messages
      :desc "General Insert"               "i g" #'+jg-ivy-general-insert
      )

(global-set-key [remap quit-window] #'+popup/quit-window)
