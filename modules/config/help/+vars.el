;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-help-map (make-sparse-keymap))
(defvar jg-info-map (make-sparse-keymap))

(add-hook 'jg-ui-transient-toggles-hook #'+jg-help-build-transient)

(speckler-add! fold ()
  `(helpful
    :modes (helpful-mode)
    :triggers (:open-all  #'hs-show-all
               :close-all #'hs-hide-all
               :toggle    #'hs-toggle-hiding
               :open      #'hs-show-block
               :open-rec  nil
               :close     #'hs-hide-block
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
