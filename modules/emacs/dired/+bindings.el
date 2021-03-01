;;; emacs/dired/+bindings.el -*- lexical-binding: t; -*-

;;;###package dired-git-info
(map! :after dired
      :map (dired-mode-map ranger-mode-map)
      :ng ")"                               #'dired-git-info-mode
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
      (:when (featurep! :util jg-tag)
       :localleader
       (:prefix "d"
          :desc "Count Untagged Orgs" "u"   #'jg-tag-dired-directory-count-untagged
          :desc "Describe Marked Tags" "t"  #'jg-tag-describe-marked-tags
         )
       (:prefix ("K" . "Destructive")
        :desc "Clean Marked" "c"          #'jg-tag-clean-marked-files
        :desc "Chop File Names" "C"       #'jg-tag-chop-long-files-from-dired
        :desc "Unify Pdf Locations" "U"   #'jg-tag-unify-pdf-locations
        :desc "Quick Compress" "Z"        #'jg-tag-quick-compress-orgs
        :desc "Reformat Json" "J"         #'jg-tag-reformat-jsons
        )
       (:prefix ("m" . "Mark")
        :desc "Mark Untagged Orgs" "u"    #'jg-tag-mark-untagged-orgs
        )
       (:prefix ("f" . "Find")
        :desc "Find Random Marked" "r"    #'jg-tag-find-random-marked-file
        :desc "Display Tag Selection" "s" #'jg-tag-display-selection
        )
       (:prefix ("g" . "gtags")
        )
       (:prefix ("i" . "Index")
        :desc "Index People" "p"          #'jg-tag-index-people
        :desc "Index Tags" "t"            #'jg-tag-index-tags
        )
       (:when (featurep! :lang python)
        "v" 'pyvenv-activate
        )
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
