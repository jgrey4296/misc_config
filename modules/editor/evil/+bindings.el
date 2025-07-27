;;; emacs/bindings/+evil-maps.el -*- lexical-binding: t; -*-
;; Reminder: evil-mode-map-alist

(dlog! "Setting up Evil Bindings: %s" (current-time-string))
;;-- setup

(defvar jg-binding-insert-state-map             (copy-keymap evil-insert-state-map))

(defvar jg-binding-replace-state-map            (make-keymap))
(set-keymap-parent jg-binding-replace-state-map jg-binding-insert-state-map)

(defvar jg-binding-motion-state-map             (make-sparse-keymap "JG map replacing evil-motion-state-map"))

(defvar jg-binding-normal-state-map             (make-sparse-keymap "JG map replacing evil-normal-state-map"))

(defvar jg-binding-operator-state-map           (make-sparse-keymap "JG map replacing evil-operator-state-map"))

(defvar jg-binding-visual-state-map             (make-sparse-keymap "JG map replacing evil-visual-state-map"))

(defvar jg-binding-jump-map                     (make-sparse-keymap))

(defvar jg-binding-backward-operator-motion-map (make-sparse-keymap))

(defvar jg-binding-forward-operator-motion-map  (make-sparse-keymap))

(defvar jg-binding-backward-general-motion-map  (make-sparse-keymap))

(defvar jg-binding-forward-general-motion-map   (make-sparse-keymap))

(defvar jg-binding-inner-text-objects-map       (make-sparse-keymap))

(defvar jg-binding-outer-text-objects-map       (make-sparse-keymap))

(defvar jg-binding-helm-map                     (make-sparse-keymap))

(defvar jg-binding-operator-map                 (make-sparse-keymap))

(defvar jg-binding-vision-map                   (make-sparse-keymap))

(defvar jg-binding-change-map                   (make-sparse-keymap))

(suppress-keymap jg-binding-motion-state-map)
;;-- end setup

(map! :map jg-binding-change-map
      (:prefix ("w" . "words"))
      (:prefix ("e" . "encode"))
      (:prefix ("i" . "lines"))
      (:prefix ("o" . "text"))
      )

(local-load! "+insert.el")
(local-load! "+jump.el")
(local-load! "+motion.el")
(local-load! "+normal.el")
(local-load! "+operator.el")
(local-load! "+text-objs.el")
(local-load! "+vision.el")
(local-load! "+visual.el")

;;-- stitching together
(map! :map jg-binding-normal-state-map
      :desc "Do Ops"        "g"   jg-binding-operator-map
      :desc "Visual Ops"    "z"   jg-binding-vision-map
      :desc "B Motion"      "["   jg-binding-backward-general-motion-map
      :desc "F Motion"      "]"   jg-binding-forward-general-motion-map
      :desc "Jumping"       "s"   jg-binding-jump-map
      :desc "Change"        "c"   jg-binding-change-map
      )

(map! :map jg-binding-visual-state-map
      :desc "Do Ops"       "g"  jg-binding-operator-map
      :desc "Visual Ops"   "z"  jg-binding-vision-map
      :desc "Inner Select" "i"  jg-binding-inner-text-objects-map
      :desc "Outer Select" "o"  jg-binding-outer-text-objects-map
      :desc "Jumping"      "s"  jg-binding-jump-map
      :desc "Change"       "c"  jg-binding-change-map
      )

(map! :map jg-binding-motion-state-map
      :desc "Backward Motion Op"  "["  jg-binding-backward-operator-motion-map
      :desc "Forward Motion Op"   "]"  jg-binding-forward-operator-motion-map
      )

(map! :map jg-binding-operator-state-map
      :desc "Backward Motion Op"  "["  jg-binding-backward-operator-motion-map
      :desc "Forward Motion Op"   "]"  jg-binding-forward-operator-motion-map
      :desc "Inner Select"        "i"  jg-binding-inner-text-objects-map
      :desc "Outer Select"        "o"  jg-binding-outer-text-objects-map
      )

;; Override default evil maps
(dlog! "Finalising Evil bindings: %s" (current-time-string))
;; Override
(setq evil-normal-state-map       jg-binding-normal-state-map
      evil-insert-state-map       jg-binding-insert-state-map
      evil-replace-state-map      jg-binding-replace-state-map
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

;;-- end stitching together

(map! :map evil-insert-state-map
      "£" (cmd! (insert "#"))
      "#" (cmd! (insert "£"))
      )

(map! :leader
      :desc "Search/Jump"  "s"    jg-binding-jump-map
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
