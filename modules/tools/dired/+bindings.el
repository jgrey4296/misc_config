;;; emacs/dired/+bindings.el -*- lexical-binding: t; -*-

(dlog! "Setting up Dired bindings")

(local-load! "+bind-changes")
;; (suppress-keymap jg-dired-mode-map)
;; (evil-make-intercept-map jg-dired-mode-map)
(with-state! 'normal #'+jg-ivy-similar-buffer)

(set-keymap-parent jg-dired-change-map jgb-change--root-map)

(map! :map jg-dired-mode-map ;; groups
      (:prefix ("M" . "Mark"))
      (:prefix "]")
      (:prefix "[")
      (:prefix ("c" . "Change"))
      (:prefix ("d" . "Describe")
       (:prefix ("?" . "Disassembly"))
       )
      (:prefix ("o" . "Open"))

      (:prefix ("e" . "Encryption")
       (:prefix ("k" . "Keys"))
       )
      )

(map! :map jg-dired-mode-map ;; localeader groups
      :localleader
      (:prefix ("f" . "Find"))
      (:prefix ("g" . "Generate"))
      (:prefix ("K" . "Destructive"))

      )

(map! :map jg-dired-mode-map ;; main
      :n "?"    #'+jg-dired-group-helper
      :n "DEL"  #'dired-kill-subdir
      :n "RET"  #'dired-find-file
      :nv "q"   #'+jg-dired-kill-subdir-or-close-buffer
      :n "!"    #'dired-do-shell-command
      :n "@"    #'dired-do-async-shell-command
      :n "#"    #'+jg-dired-seq-command
      :n "S"    #'hydra-dired-quick-sort/body
      :n "."    #'dired-omit-mode
      :n "n"    #'evil-ex-search-next
      :n "v"    #'evil-visual-state
      :n "g"    #'revert-buffer
      :n ","    #'+jg-ivy-similar-buffer--with-state-normal
      :desc "Delete"         :n "D"    #'+jg-dired-async-trash
      :desc "Touch"          :n "="    #'+jg-dired-touch
      :desc "Expand Subdir"  :n "i"    #'+jg-dired-insert-subdir-maybe-recursive
      :desc "Expand Marked"  :n "I"    #'+jg-dired-insert-marked-subdir
      :n "y"    #'dired-copy-filename-as-kill
      :n "Y"    (cmd! (dired-copy-filename-as-kill 0))
      :desc "Fd File"        :n "s f"  #'fd-dired
      :desc "Related"        :n "s \\" #'+jg-projects-find-related
      :n "c" jg-dired-change-map
      )

(map! :map jg-dired-mode-map ;; mark
      :n "t"                                      #'dired-toggle-marks
      :n "m"                                      #'dired-mark
      :n "u"                                      #'dired-unmark
      :n "U"                                      #'dired-unmark-all-marks

      (:prefix "M"
      :desc "flag garbage files"           :n "x" #'dired-flag-garbage-files
      :desc "mark files containing regexp" :n "g" #'dired-mark-files-containing-regexp
      :desc "mark files regexp"            :n "m" #'dired-mark-files-regexp
      :desc "mark symlinks"                :n "s" #'dired-mark-symlinks
      :desc "mark hash duplicates"         :n "h" #'+jg-dired-hash-duplicates
      )
)

(map! :map jg-dired-mode-map ;; movement
      :n "j"                          #'dired-next-line
      :n "k"                          #'dired-previous-line

      :n "J"                          #'dired-next-dirline
      :n "K"                          #'dired-prev-dirline

      :n "h" #'dired-up-directory
      :n "l" #'dired-next-marked-file
      :n "L" #'dired-prev-marked-file

      (:prefix "]"
       :desc "Next Marked" :n "m"  #'dired-next-marked-file
       )
      (:prefix "["
       :desc "Prev Marked" :n "m"  #'dired-prev-marked-file
       )
)


(map! :map jg-dired-mode-map ;; describe
      :prefix "d"
      :desc "Diff Dirs"        :n "TAB" #'dired-compare-directories
      :desc "Marked Count"     :n "c" #'+jg-dired-marked-info
      :desc "Metadata"         :n "m" #'+jg-dired-exiftool-files
      :desc "Viruses"          :n "v" #'+jg-dired-scan-files
      :desc "Diff"             :n "d" #'dired-diff
      :desc "Diff Directories" :n "D" #'ediff-directories
      :desc "Hash"             :n "h" #'+jg-dired-hash-files
      :desc "Marked Size"      :n "s" #'+jg-dired-dir-size
      :desc "Hide Details"     :n "i" 'dired-hide-details-mode
      :desc "Git Info"         :n "g" #'dired-git-info-mode
      :desc "File Path"        :n "p" (cmd! (dired-copy-filename-as-kill 0))
      :desc "File Name"        :n "n" #'dired-copy-filename-as-kill
      :desc "Zip File List"    :n "l" #'+jg-dired-async-list-zip-files
      )

(map! :map jg-dired-mode-map ;; open
      :prefix "o"
      :desc "Marked Files"        :n "m" #'dired-do-find-marked-files
      :desc "Fundamental"         :n "f" #'+jg-dired-find-literal
      :desc "Other Window"        :n "o" #'dired-find-file-other-window
      :desc "Quicklook"           :n "l" #'+jg-dired-quick-look
      :desc "Find Random Marked"  :n "r" #'+jg-dired-find-random-marked-file
      :desc "Eww"                 :n "e" #'eww-open-file
      )

(map! :map jg-dired-mode-map ;; encryption
      (:prefix "e"
       :desc "Decrypt"  :n   "d" #'epa-dired-do-decrypt
       :desc "Encrypt"  :n   "e" #'epa-dired-do-encrypt
       :desc "Sign"     :n   "s" #'epa-dired-do-sign
       :desc "Verify"   :n   "v" #'epa-dired-do-verify
       (:prefix "k"
        :desc "List Keys"   :n "l" #'+jg-dired-epa-list-keys
        :desc "Import Keys" :n "i" #'epa-import-keys
        :desc "Export Keys" :n "E" #'+jg-dired-epa-export-keys
        )
       )
      )

(map! :map jg-dired-mode-map ;; localleader
      :localleader
      :desc "Find Marked Files" "RET" #'dired-do-find-marked-files
      :desc "Start Server" "S" #'+jg-dired-async-server

      (:prefix "f"
       :desc "Find Marked Files" "f" #'dired-do-find-marked-files
       :desc "Literally"         "l" #'+jg-dired-find-literal
       )
      )

(map! :map jgb-jump--root-map
      :desc "Fd Find" "/ F"  #'fd-dired
      )

(setq dired-mode-map jg-dired-mode-map)

(map! :leader
      "f t" #'+jg-list-trash
      "d"   #'+jg-dired-popup
      )

(when (modulep! +dirvish) (local-load! "+bind-dirvish"))
(provide 'jg-dired-bindings)
