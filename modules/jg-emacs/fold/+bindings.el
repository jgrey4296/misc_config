;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :after jg-leader-bindings-loaded
      :desc "Jump to Auto-hide-heading" :n "j h" #'autohide-minor-mode-fold-jump-to-heading
      )

(map! :map jg-binding-normal-state-map
      :after jg-evil-bindings
      :desc "Toggle" "a" #'evil-toggle-fold
      )

(map! :map jg-binding-vision-map
      :after jg-evil-bindings
      :desc "Insert Fold block" "1" #'autohide-minor-mode-wrap-block

      :desc "open-fold-rec" "A"   #'evil-open-fold-rec
      :desc "toggle-fold"   "a"   #'evil-toggle-fold
      :desc "open-folds"    "o"   #'evil-open-folds
      :desc "Close Folds"   "m"   #'evil-close-folds

      :desc "Refold"        "r"   #'autohide-minor-mode-run-folds
      :desc "Next Fold"     "j"   #'+fold/next
      :desc "Prev Fold"     "k"   #'+fold/previous
)
(map! :map jg-binding-vision-map
      :after (vimish-fold jg-evil-bindings)
      :prefix ("v" . "Vimish Fold")
       :desc "toggle-all"             "A"  #'vimish-fold-toggle-all
       :desc "toggle"                 "a"  #'vimish-fold-toggle
       :desc "close folds"            "m"  #'vimish-fold-refold-all
       :desc "open-folds"             "o"  #'vimish-fold-unfold-all

       :desc "delete-all"             "D"  #'vimish-fold-delete-all
       :desc "delete"                 "d"  #'vimish-fold-delete

       :desc "fold"                   "f"  #'vimish-fold

       :desc "next-fold"              "j"  #'vimish-fold-next-fold
       :desc "previous-fold"          "k"  #'vimish-fold-previous-fold
      )
