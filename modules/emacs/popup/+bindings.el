;;; +bindings.el -*- lexical-binding: t; -*-

(defvar jg-popup-neotree-mode-map (make-keymap))

(map! :leader
      :desc "Popup Buffer"          "<"     #'+jg-popup-ivy-buffer
      :n "w s" #'neotree-toggle
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

(map! :leader
      :prefix "p"
      :n "RET" #'+neotree/find-this-file
      )

(map! :map jg-popup-neotree-mode-map
      :n "i"  #'ignore
      :n "g"  #'neotree-refresh
      :n "q"  #'neotree-hide
      :n "."  #'neotree-hidden-file-toggle
      :n "\\" #'neotree-change-root
      :n "r"  #'neotree-rename-node

      :n "h"  #'+neotree/collapse-or-up
      :n "l"  #'+neotree/expand-or-open
      :n "H"  #'neotree-select-up-node
      :n "L"  #'neotree-select-down-node
      :n "n"  #'neotree-select-next-sibling-node
      :n "N"  #'neotree-select-previous-sibling-node

      :n "RET" (neotree-make-executor :file-fn 'neo-open-file :dir-fn  'neo-open-dir)
      )

(setq neotree-mode-map jg-popup-neotree-mode-map)
