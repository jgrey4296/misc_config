;;; emacs/dired/+bindings.el -*- lexical-binding: t; -*-

(message "Setting up Dired bindings")

(map! :map dired-mode-map ;; main
      :n  "\\" #'+jg-dired-hash-files
      :nv ")" #'dired-git-info-mode
      :n  "o" #'dired-find-file-other-window
      :n  "S" #'hydra-dired-quick-sort/body
      :nv "j" #'dired-next-line
      :nv "k" #'dired-previous-line
      :nv "J" #'dired-next-dirline
      :nv "K" #'dired-prev-dirline
      :nv "n" #'evil-ex-search-next
      :nv "N" #'evil-ex-search-previous

      :n "v" #'ignore
      :n ":" #'ignore

      :nv "q" #'+jg-dired-kill-subdir-or-close-buffer
      :desc "Remove Subdir"        :n "DEL"                            #'dired-kill-subdir

      "C-h" #'dired-up-directory

      :desc "Expand Subdir"        :n "i"                              #'+jg-dired-insert-subdir-maybe-recursive
      :desc "Expand Marked"        :n "I"                              #'+jg-dired-insert-marked-subdir

      (:prefix "]"
       :desc "Next Marked" :n "m" #'dired-next-marked-file
       )
      (:prefix "["
       :desc "Prev Marked" :n "m" #'dired-prev-marked-file
       )
)

(map! :map dired-mode-map ;; localleader
      :localleader
      :desc "Hide Toggle"           "h" #'dired-omit-mode
      :desc "Symlink"               "S" #'dired-do-symlink
      :desc "Quicklook"             "l" #'+jg-dired-quick-look
      :desc "DropWatch"             "w" (cmd! (shell-command "dropbox_watcher"))
      (:prefix ("d" . "Describe")
       :desc "Summarise Orgs"       "s" #'+jg-dired-create-summary-of-orgs
       :desc "Marked Info"          "m" #'+jg-dired-marked-info
       :desc "Dired Diff"           "d" #'+jg-dired-diff
       )
      (:prefix ("K" . "Destructive")
       :desc "lower case files"   "1"   #'+jg-dired-downcase-marked-files
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

(map! :map dired-mode-map ;; dired-do
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
      :desc "Global Match Rename"          :n "R" #'+jg-dired-GLOBAL-do-rename-regexp
      :desc "upcase"                       :n "u" #'dired-upcase
      )

(map! :map dired-mode-map ;; encryption
      :after epa
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

(map! :after dired
      :map (dired-mode-map ranger-mode-map)
      :ng ")" #'dired-git-info-mode)

(map! :map dirvish-mode-map
      :n "b" #'dirvish-goto-bookmark
      :n "z" #'dirvish-show-history
      :n "f" #'dirvish-file-info-menu
      :n "F" #'dirvish-toggle-fullscreen
      :n "l" #'dired-find-file
      :n "h" #'dired-up-directory
      :localleader
      "h" #'dired-omit-mode)

(evil-make-overriding-map dired-mode-map)

(provide 'jg-dired-bindings)