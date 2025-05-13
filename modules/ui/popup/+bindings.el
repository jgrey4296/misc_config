;;; +bindings.el -*- lexical-binding: t; -*-


(map! :leader
      :desc "Popup Buffer"          "<"     #'+jg-popup-ivy-buffer
      :desc "Toggle last popup"     "`"   #'+popup/toggle
      :desc "Toggle last popup"     "z"   #'+popup/toggle
      )

(map! :map +popup-buffer-mode-map
      :n "q" #'+popup/close
      )

(map! :map jg-help-map
      :after jg-help-bindings
      "u ?" #'+popup/diagnose
      )

(map! :leader
      :desc "SCRATCH"                      "6" (cmd! (+jg-popup-ivy-open "*scratch*"))
      :desc "Messages"                     "0" #'+jg-popup-messages
      )

(setq popup-menu-keymap (make-keymap))
(map! :map popup-menu-keymap
      "j" 'popup-next
      "k" 'popup-previous
      "q" #'keyboard-quit
      "RET" 'popup-select
      "l" 'popup-select

      "\\" 'popup-isearch
      "?"  'popup-help
      )


(global-set-key [remap quit-window] #'+popup/quit-window)
