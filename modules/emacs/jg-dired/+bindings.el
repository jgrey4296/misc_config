;;; emacs/dired/+bindings.el -*- lexical-binding: t; -*-

(message "Setting up Dired bindings")

(map! :after evil
      :map dired-mode-map
      :nv ")" #'dired-git-info-mode
      :n "o"  #'dired-find-file-other-window
      :n "S"  #'hydra-dired-quick-sort/body
      :nv "j" #'dired-next-line
      :nv "k" #'dired-previous-line
      :nv "J" #'dired-next-dirline
      :nv "K" #'dired-prev-dirline
      :nv "q" #'kill-current-buffer
      :nv "Q" #'+dired/quit-all
      :nv "n" #'evil-ex-search-next
      :nv "N" #'evil-ex-search-previous

      :n "v" nil
      :n "i" nil
      :n "I" nil
      :n ":" nil

      "C-h" #'dired-up-directory

      :desc "Expand Subdir"        :n "i"                              #'+jg-dired-insert-subdir-maybe-recursive
      :desc "Expand Marked"        :n "I"                              #'+jg-dired-insert-marked-subdir
      :desc "Remove Subdir"        :n "DEL"                            #'dired-kill-subdir

      (:prefix "]"
       :desc "Next Marked" :n "m" #'dired-next-marked-file
       )
      (:prefix "["
       :desc "Prev Marked" :n "m" #'dired-prev-marked-file
       )

      (:prefix "%"
       :desc "Global Match Rename" :n "R" #'+jg-GLOBAL-dired-do-rename-regexp)
      )

(map! :after evil
      :map dired-mode-map
      :localleader
      :desc "Hide Toggle"           "h"         #'dired-omit-mode
      :desc "Symlink"               "S"         #'dired-do-symlink
      :desc "Quicklook"             "l"         #'+jg-dired-quick-look
      (:prefix ("d" . "Describe")
       :desc "Summarise Orgs"     "s"     #'+jg-dired-create-summary-of-orgs
       :desc "Marked Info"        "m"        #'+jg-dired-marked-info
       :desc "Dired Diff"         "d"         #'+jg-dired-diff
       )
      (:prefix ("K" . "Destructive")
       :desc "Reformat jsons"     "J"   #'+jg-dired-reformat-jsons
       :desc "Replace regexp"     "R"   #'dired-do-find-regexp-and-replace
       )
      (:prefix ("f" . "Find")
       :desc "Find Random Marked" "r" #'+jg-dired-find-random-marked-file
       :desc "Fundamental"        "f" #'+jg-dired-find-literal
       )
      (:prefix ("m" . "Mark")
       :desc "Symlinks"           "S"    #'dired-mark-symlinks
       )
      )

(map! :after evil
      :map dired-mode-map
      :prefix ("%" . "Dired-Do")
      :desc "flag-garbage-files"           :n "&" #'dired-flag-garbage-files
      :desc "do-copy-regexp"               :n "C" #'dired-do-copy-regexp
      :desc "do-hardlink-regexp"           :n "H" #'dired-do-hardlink-regexp
      :desc "do-rename-regexp"             :n "R" #'dired-do-rename-regexp
      :desc "do-symlink-regexp"            :n "S" #'dired-do-symlink-regexp
      :desc "do-relsymlink-regexp"         :n "Y" #'dired-do-relsymlink-regexp
      :desc "flag-files-regexp"            :n "d" #'dired-flag-files-regexp
      :desc "mark-files-containing-regexp" :n "g" #'dired-mark-files-containing-regexp
      :desc "downcase"                     :n "l" #'dired-downcase
      :desc "mark-files-regexp"            :n "m" #'dired-mark-files-regexp
      :desc "do-rename-regexp"             :n "r" #'dired-do-rename-regexp
      :desc "upcase"                       :n "u" #'dired-upcase
      )

(map! :after (evil epa)
      :map dired-mode-map
      ";" nil
      (:prefix (";" . "Encryption")
       :desc "Decrypt" "d" #'epa-dired-do-decrypt
       :desc "Encrypt" "e" #'epa-dired-do-encrypt
       :desc "Sign"    "s" #'epa-dired-do-sign
       :desc "Verify"  "v" #'epa-dired-do-verify
       :desc "List Keys" "l" #'+jg-dired-epa-list-keys
       :desc "Import Keys" "i" #'epa-import-keys

       (:prefix ("K" . "EXPORT")
        :desc "Keys" "k" #'+jg-dired-epa-export-keys
        )
       )
      )

(evil-make-overriding-map dired-mode-map)
