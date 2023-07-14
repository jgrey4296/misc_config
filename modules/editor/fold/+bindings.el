;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-jump-map
      :desc "Jump to Auto-hide-heading" :n "h" #'code-shy-fold-jump-to-heading
      )

(map! :map jg-binding-normal-state-map
      :desc "Toggle" "a" #'evil-toggle-fold
      :desc "open-fold-rec" "A"   #'evil-open-fold-rec
      )

(map! :map jg-binding-vision-map
      "?" #'+jg-fold/debug
      :desc "Insert Fold block" "1" #'code-shy-wrap-block

      :desc "open-fold-rec" "A"   #'evil-open-fold-rec
      :desc "toggle-fold"   "a"   #'evil-toggle-fold
      :desc "open-folds"    "o"   #'evil-open-folds
      :desc "Close Folds"   "m"   #'evil-close-folds

      :desc "Refold"        "r"   #'code-shy-run-folds
      :desc "Next Fold"     "j"   #'+fold/next
      :desc "Prev Fold"     "k"   #'+fold/previous

      ;; TODO fold string
)
(map! :map jg-binding-vision-map
      :after vimish-fold
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

(map! :map jg-binding-forward-operator-motion-map
      :desc "Fold Block"               "1" #'code-shy-forward-block
      )

(map! :map jg-binding-backward-operator-motion-map
      :desc "Fold Block"               "1" #'code-shy-backward-block
)

(map! :map jg-help-map
      :after jg-help-binding
      "d f" #'+jg-fold/debug
      )
