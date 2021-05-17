;;; emacs/dired/+bindings.el -*- lexical-binding: t; -*-


(map! :after dired
      :map dired-mode-map
      :n ")" #'dired-git-info-mode
      :n "o" #'dired-find-file-other-window
      :n "S" #'hydra-dired-quick-sort/body
      :n "j" #'dired-next-line
      :n "k" #'dired-previous-line
      :n "J" #'dired-next-dirline
      :n "K" #'dired-prev-dirline
      :n "q" #'kill-current-buffer
      :n "Q" #'+dired/quit-all

      :n "v" nil
      :n "i" nil
      ":" nil

      "C-h" #'dired-up-directory

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
       :desc "Replace regexp"   "R" #'dired-do-find-regexp-and-replace
       )
      (:prefix ("f" . "Find")
       :desc "Find Random Marked" "r" #'+jg-dired-find-random-marked-file
       :desc "Fundamental"        "f" #'+jg-dired-find-literal
       )
      )

(map! :map dired-mode-map
      :prefix ("%" . "Dired-Do")
      :desc "flag-garbage-files"           :n "&" #'dired-flag-garbage-files
      :desc "do-copy-regexp"               :n "C" #'dired-do-copy-regexp
      :desc "do-hardlink-regexp"           :n "H" #'dired-do-hardlink-regexp
      :desc "do-rename-regexp"             :n "R" #'dired-do-rename-regexp
      :desc "do-symlink-regexp"            :n "S" #'dired-do-symlink-regexp
      :desc "do-relsymlink-regexp"         :n "Y" #'dired-do-relsymlink-regexp
      :desc "flag-files-regexp"            :n "d" #'dired-flag-files-regexp
      :desc "mark-files-containing-regexp"  :n "g" #'dired-mark-files-containing-regexp
      :desc "downcase"                     :n "l" #'dired-downcase
      :desc "mark-files-regexp"            :n "m" #'dired-mark-files-regexp
      :desc "do-rename-regexp"             :n "r" #'dired-do-rename-regexp
      :desc "upcase"                       :n "u" #'dired-upcase
      )

(evil-make-overriding-map dired-mode-map)
