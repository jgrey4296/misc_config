;;; +bindings.el -*- lexical-binding: t; -*-


(map! :after (vimish-fold jg-evil-bindings)
      :map jg-binding-vision-map
      :prefix ("v" . "Vimish Fold")
       :desc "toggle-all"             "A"  #'vimish-fold-toggle-all
       :desc "delete-all"             "D"  #'vimish-fold-delete-all
       :desc "toggle"                 "a"  #'vimish-fold-toggle
       :desc "delete"                 "d"  #'vimish-fold-delete
       :desc "fold"                   "f"  #'vimish-fold
       :desc "next-fold"              "j"  #'vimish-fold-next-fold
       :desc "previous-fold"          "k"  #'vimish-fold-previous-fold
       :desc "refold-all"             "m"  #'vimish-fold-refold-all
       :desc "unfold-all"             "r"  #'vimish-fold-unfold-all
      )
