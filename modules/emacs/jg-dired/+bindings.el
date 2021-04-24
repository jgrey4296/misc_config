;;; emacs/dired/+bindings.el -*- lexical-binding: t; -*-

(message "Loading modules/emacs/dired/+bindings.el")

;;;###package dired-git-info
(map! :after dired
      :map (dired-mode-map ranger-mode-map)
      :m ")"                                #'dired-git-info-mode
      :n "o"                                #'dired-find-file-other-window
      :n "S"                                #'hydra-dired-quick-sort/body
      (:prefix "]"
       :desc "Next Marked" :n "m" #'dired-next-marked-file
       )
      (:prefix "["
       :desc "Prev Marked" :n "m" #'dired-prev-marked-file
       )
      (:when (featurep! :editor jg-personal)
        :n "i"                              #'+jg-personal-dired-insert-subdir-maybe-recursive
        :n "DEL"                            #'dired-kill-subdir
        )
      (:when (featurep! :lang python)
       "v" 'pyvenv-activate
       )
      )

(map! :map dired-mode-map
       :localleader
       (:prefix ("d" . "Describe")
        :desc "Summarise Orgs" "s"         #'+jg-personal-dired-create-summary-of-orgs
        :desc "Marked Info" "m"            #'+jg-personal-dired-marked-info
        :desc "Dired Diff" "d"             #'+jg-personal-dired-diff
        )
       (:prefix ("K" . "Destructive")
        :desc "Reformat jsons"   "J" #'+jg-dired-reformat-jsons
        )
      )

(map! :after dired-aux
      :map dired-mode-map
      :prefix "%"
      :desc "Global Match Rename" :n "R" #'+jg-GLOBAL-dired-do-rename-regexp
      )

;; Dired bindings
(map! :after dired
      :map (dired-mode-map ranger-mode-map)
      :localleader
      (:prefix ("f" . "Find")
       :desc "Find Random Marked" "r" #'+jg-dired-find-random-marked-file
       )
      )
