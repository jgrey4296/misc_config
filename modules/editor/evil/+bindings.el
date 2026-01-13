;;; emacs/bindings/+evil-maps.el -*- lexical-binding: t; -*-
;; Reminder: evil-mode-map-alist

(dlog! "Setting up Evil Bindings: %s" (current-time-string))

(suppress-keymap jge-motion-state-map)

(local-load! "+bind-insert.el")
(local-load! "+bind-jump.el")
(local-load! "+bind-motion.el")
(local-load! "+bind-normal.el")
(local-load! "+bind-operator.el")
(local-load! "+bind-text-objs.el")
(local-load! "+bind-vision.el")
(local-load! "+bind-visual.el")

;;-- stitching together
(map! :map jge-normal-state-map
      :desc "Do Ops"        "g"   jge-operator-map
      :desc "Visual Ops"    "z"   jgb-vision--root-map
      :desc "Jumping"       "s"   jgb-jump--root-map
      )

(map! :map jge-visual-state-map
      :desc "Do Ops"       "g"  jge-operator-map
      :desc "Visual Ops"   "z"  jgb-vision--root-map
      :desc "Inner Select" "i"  jge-txtobj--inner-map
      :desc "Outer Select" "o"  jge-txtobj--outer-map
      :desc "Jumping"      "s"  jgb-jump--root-map
      )

(map! :map jge-motion-state-map
      :desc "Backward Motion Op"  "["  jge-motion--op-bw-map
      :desc "Forward Motion Op"   "]"  jge-motion--op-fw-map
      )

(map! :map jge-operator-state-map
      :desc "Backward Motion Op"  "["  jge-motion--op-bw-map
      :desc "Forward Motion Op"   "]"  jge-motion--op-fw-map
      :desc "Inner Select"        "i"  jge-txtobj--inner-map
      :desc "Outer Select"        "o"  jge-txtobj--outer-map
      )

;; Override default evil maps
(dlog! "Finalising Evil bindings: %s" (current-time-string))

;; Override
(setq evil-normal-state-map       jge-normal-state-map
      evil-insert-state-map       jge-insert-state-map
      evil-replace-state-map      jge-replace-state-map
      evil-visual-state-map       jge-visual-state-map
      evil-operator-state-map     jge-operator-state-map
      evil-motion-state-map       jge-motion-state-map
      evil-inner-text-objects-map jge-txtobj--inner-map
      evil-outer-text-objects-map jge-txtobj--outer-map
      )

;; Refresh
(setq evil-global-keymaps-alist
      '((evil-emacs-state-minor-mode    . evil-emacs-state-map)
        (evil-motion-state-minor-mode   . evil-motion-state-map)
        (evil-replace-state-minor-mode  . evil-replace-state-map)
        (evil-operator-state-minor-mode . evil-operator-state-map)
        (evil-visual-state-minor-mode   . evil-visual-state-map)
        (evil-insert-state-minor-mode   . evil-insert-state-map)
        (evil-normal-state-minor-mode   . evil-normal-state-map)
        )
      )

;;-- end stitching together

(global-set-key (kbd "<backtab>")       #'evil-normal-state)

(map! :leader
      :desc "Search/Jump"  "s"    jgb-jump--nontext-map
      :desc "Evil States"  "a"    #'+jg-evil-state-ivy
      :desc "Record Macro" "SPC"  #'evil-record-macro
      :desc "Switch to last buffer" "TAB" #'evil-switch-to-windows-last-buffer
      :desc "Evil ex path" "i :"  (cmd! (evil-ex "R!echo "))
      :desc "From evil register"           "i r" #'evil-show-registers

      :desc "Create Buffer"               "b c"   #'evil-buffer-new
      :desc "Save all buffers"            "b S"   #'evil-write-all

      )

(dlog! "Evil Bindings Complete: %s" (current-time-string))
(provide 'jg-evil-bindings)
