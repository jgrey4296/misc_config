;;; emacs/dired/+bindings.el -*- lexical-binding: t; -*-


(map! :map dired-mode-map
      :n ")"                                #'dired-git-info-mode
      :n "o"                                #'dired-find-file-other-window
      :n "S"                                #'hydra-dired-quick-sort/body
      :n "j" #'dired-next-line
      :n "k" #'dired-previous-line
      :n "J" #'dired-next-dirline
      :n "K" #'dired-prev-dirline

      :n "v" nil
      :n "i" nil
      ":" nil


      :desc "Expand Subdir"        :n "i"                              #'+jg-dired-insert-subdir-maybe-recursive
      :desc "Remove Subdir"        :n "DEL"                            #'dired-kill-subdir
      :desc "Activate Environment" :n "v" 'pyvenv-activate

      (:prefix "]"
       :desc "Next Marked" :n "m" #'dired-next-marked-file
       )
      (:prefix "["
       :desc "Prev Marked" :n "m" #'dired-prev-marked-file
       )

      (:prefix "%"
       :desc "Global Match Rename" :n "R" #'+jg-GLOBAL-dired-do-rename-regexp)
      )

(map! :map dired-mode-map
      :localleader
      :desc "Hide Toggle" "h" #'dired-omit-mode
      (:prefix ("d" . "Describe")
       :desc "Summarise Orgs" "s"         #'+jg-dired-create-summary-of-orgs
       :desc "Marked Info" "m"            #'+jg-dired-marked-info
       :desc "Dired Diff" "d"             #'+jg-dired-diff
       )
      (:prefix ("K" . "Destructive")
       :desc "Reformat jsons"   "J" #'+jg-dired-reformat-jsons
       )
      (:prefix ("f" . "Find")
       :desc "Find Random Marked" "r" #'+jg-dired-find-random-marked-file
       :desc "Fundamental"        "f" #'+jg-dired-find-literal
       )
      )

(evil-make-overriding-map dired-mode-map)
