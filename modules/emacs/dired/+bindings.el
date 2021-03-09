;;; emacs/dired/+bindings.el -*- lexical-binding: t; -*-

;;;###package dired-git-info
(map! :after dired
      :map (dired-mode-map ranger-mode-map)
      :m ")"                                #'dired-git-info-mode
      :n "o"                                #'dired-find-file-other-window
      :n "S"                                #'hydra-dired-quick-sort/body
      (:when (featurep! :editor jg-personal)
        :n "i"                              #'+jg-personal-dired-insert-subdir-maybe-recursive
        :n "DEL"                            #'dired-kill-subdir
       (:localleader
        (:prefix ("d" . "Describe")
         :desc "Summarise Orgs" "s"         #'+jg-personal-dired-create-summary-of-orgs
         :desc "Marked Info" "m"            #'+jg-personal-dired-marked-info
         :desc "Dired Diff" "d"             #'+jg-personal-dired-diff
         )
        )
       )
      (:when (featurep! :lang python)
       "v" 'pyvenv-activate
       )
      )

(map! :leader
      :prefix "t"
      :desc "Toggle Dired-Omit-Mode" "o" #'dired-omit-mode
      )

(map! :after dired-aux
      :map dired-mode-map
      :prefix "%"
      :desc "Global Match Rename" :n "R" #'+jg-GLOBAL-dired-do-rename-regexp
      )
