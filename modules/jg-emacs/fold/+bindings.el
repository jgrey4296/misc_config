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

(map! :after jg-lead-bindings-loaded
      :desc "Auto-Hide"      :n "t h"   #'+jg-fold-toggle-auto-hide
      )
;; (map! :map jg-binding-vision-map
;;       :after jg-evil-bindings
;;       :desc "Insert Fold block" "1" #'+jg-fold-wrap-block

;;       :desc "open-fold-rec" "A"   #'evil-open-fold-rec
;;       :desc "toggle-fold"   "a"   #'evil-toggle-fold
;;       :desc "close-fold"    "d"   #'evil-close-fold
;;       :desc "open-folds"    "o"   #'evil-open-folds
;;       :desc "Close Folds"   "c"   #'evil-close-folds

;;       :desc "Refold"        "r"   (cmd! (+jg-fold-auto-hide))
;;       :desc "Next Fold"     "j"   #'+fold/next
;;       :desc "Prev Fold"     "k"   #'+fold/previous
;; )
