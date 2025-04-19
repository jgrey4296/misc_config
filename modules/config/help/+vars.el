;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-help-map (make-sparse-keymap))
(defvar jg-info-map (make-sparse-keymap))

(add-hook 'jg-ui-transient-toggles-hook #'+jg-help-build-transient)

(speckler-add! fold ()
  `(helpful
    :modes (helpful-mode)
    :triggers (:open-all   #'outline-show-all
               :close-all  #'jg-fold-outline-hide-sublevels
               :toggle     #'outline-toggle-children
               :open       #'jg-fold-outline-show-children
               :open-rec   #'outline-show-subtree
               :close      #'outline-hide-subtree
               )
    )
  )
(speckler-add! popup ()
  :override nil
  '(helpful
    ("\*helpful.*?\\*"             :side bottom :ttl nil :height 20 :quit t :select nil :priority 150)
    ("^\\*\\([Hh]elp\\|Apropos\\)" :slot 2 :vslot -8 :size 0.35 :select t :quit nil :priority -100)
    ("^\\*\\(?:Wo\\)?Man "         :vslot -6 :size 0.45 :select t :quit t :ttl 0 :priority -100)
    )
  )
