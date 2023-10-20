;;; emacs/bindings/+evil-maps.el -*- lexical-binding: t; -*-
;; Reminder: evil-mode-map-alist

(doom-log "Setting up Evil Bindings: %s" (current-time-string))
;;-- setup

(defvar jg-binding-insert-state-map             (copy-keymap evil-insert-state-map))

(defvar jg-binding-motion-state-map             (make-sparse-keymap "JG map replacing evil-motion-state-map"))

(defvar jg-binding-normal-state-map             (make-sparse-keymap "JG map replacing evil-normal-state-map"))

(defvar jg-binding-operator-state-map           (make-sparse-keymap "JG map replacing evil-operator-state-map"))

(defvar jg-binding-visual-state-map             (make-sparse-keymap "JG map replacing evil-visual-state-map"))

(define-prefix-command 'jg-binding-backward-operator-motion-map nil "jgb-backward-op")

(define-prefix-command 'jg-binding-forward-operator-motion-map  nil "jgb-forward-op")

(define-prefix-command 'jg-binding-backward-general-motion-map  nil "jgb-backward-motion")

(define-prefix-command 'jg-binding-forward-general-motion-map   nil "jgb-forward-motion")

(define-prefix-command 'jg-binding-inner-text-objects-map       nil "jgb-inner")

(define-prefix-command 'jg-binding-outer-text-objects-map       nil "jgb-outer")

(define-prefix-command 'jg-binding-jump-map                     nil "jgb-jump")

(define-prefix-command 'jg-binding-helm-map                     nil "jgb-helm")

(define-prefix-command 'jg-binding-operator-map                 nil "jgb-ops")

(define-prefix-command 'jg-binding-vision-map                   nil "jgb-vision")

(define-prefix-command 'jg-binding-change-map                   nil "jgb-change")

(suppress-keymap jg-binding-motion-state-map)
;;-- end setup

(map! :map jg-binding-change-map
      (:prefix ("w" . "words"))
      (:prefix ("e" . "encode"))
      (:prefix ("i" . "lines"))
      (:prefix ("o" . "text"))
      )

(local-load! "submaps/insert-bindings.el")
(local-load! "submaps/jump-bindings.el")
(local-load! "submaps/motion-bindings.el")
(local-load! "submaps/normal-bindings.el")
(local-load! "submaps/operator-bindings.el")
(local-load! "submaps/text-obj-bindings.el")
(local-load! "submaps/vision-bindings.el")
(local-load! "submaps/visual-bindings.el")

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
      :desc "Outer Select" "o"  'jg-binding-outer-text-objects-map
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
      :desc "Outer Select"        "o"  'jg-binding-outer-text-objects-map
      )

;; Override default evil maps
(doom-log "Finalising Evil bindings: %s" (current-time-string))
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

;;-- end stitching together

(map! :map evil-insert-state-map
      "£" (cmd! (insert "#"))
      "#" (cmd! (insert "£"))
      )

(doom-log "Evil Bindings Complete: %s" (current-time-string))
(provide 'jg-evil-bindings)
