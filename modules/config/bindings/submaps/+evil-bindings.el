;;; emacs/bindings/+evil-maps.el -*- lexical-binding: t; -*-
;; Reminder: evil-mode-map-alist

(message "Setting up Evil Bindings: %s" (current-time-string))
;;-- setup
(setq jg-binding-insert-state-map (copy-keymap evil-insert-state-map))

(suppress-keymap jg-binding-motion-state-map)
;;-- end setup

(load! "+evil-change-bindings.el")
(load! "+evil-insert-bindings.el")
(load! "+evil-motion-bindings.el")
(load! "+evil-normal-bindings.el")
(load! "+evil-text-obj-bindings.el")
(load! "+evil-visual-bindings.el")
(load! "+jump-bindings.el")
(load! "+op-bindings.el")
(load! "+vision-bindings.el")


;;-- stitching together
(map! :map jg-binding-normal-state-map
      :desc "Do Ops"        "g"   'jg-binding-operator-map
      :desc "Visual Ops"    "z"   'jg-binding-vision-map
      :desc "B Motion"      "["   'jg-binding-backward-general-motion-map
      :desc "F Motion"      "]"   'jg-binding-forward-general-motion-map
      :desc "Jumping"       "s"   'jg-binding-jump-map
      :desc "Change"        "c"   'jg-binding-change-map
      )

(map! :map jg-binding-visual-state-map
      :desc "Do Ops"       "g"  'jg-binding-operator-map
      :desc "Visual Ops"   "z"  'jg-binding-vision-map
      :desc "Inner Select" "i"  'jg-binding-inner-text-objects-map
      :desc "Outer Select" "a"  'jg-binding-outer-text-objects-map
      :desc "Jumping"      "s"  'jg-binding-jump-map
      :desc "Change"       "c"  'jg-binding-change-map
      )

(map! :map jg-binding-motion-state-map
      :desc "Backward Motion Op"  "["  'jg-binding-backward-operator-motion-map
      :desc "Forward Motion Op"   "]"  'jg-binding-forward-operator-motion-map
      )

(map! :map jg-binding-operator-state-map
      :desc "Backward Motion Op"  "["  'jg-binding-backward-operator-motion-map
      :desc "Forward Motion Op"   "]"  'jg-binding-forward-operator-motion-map
      :desc "Inner Select"        "i"  'jg-binding-inner-text-objects-map
      :desc "Outer Select"        "a"  'jg-binding-outer-text-objects-map
      )

;; Override default evil maps
(message "Finalising Evil bindings: %s" (current-time-string))
;; Override
(setq evil-normal-state-map       jg-binding-normal-state-map
      evil-insert-state-map       jg-binding-insert-state-map
      evil-visual-state-map       jg-binding-visual-state-map
      evil-operator-state-map     jg-binding-operator-state-map
      evil-motion-state-map       jg-binding-motion-state-map
      evil-inner-text-objects-map jg-binding-inner-text-objects-map
      evil-outer-text-objects-map jg-binding-outer-text-objects-map
      )

;; Refresh
(setq evil-global-keymaps-alist
'((evil-emacs-state-minor-mode    . evil-emacs-state-map)
  (evil-motion-state-minor-mode   . evil-motion-state-map)
  (evil-replace-state-minor-mode  . evil-replace-state-map)
  (evil-operator-state-minor-mode . evil-operator-state-map)
  (evil-visual-state-minor-mode   . evil-visual-state-map)
  (evil-insert-state-minor-mode   . evil-insert-state-map)
  (evil-normal-state-minor-mode   . evil-normal-state-map)))

(global-set-key (kbd "<backtab>")       #'evil-normal-state)

(message "Evil Bindings Complete: %s" (current-time-string))

(provide 'jg-evil-bindings)
;;-- end stitching together
