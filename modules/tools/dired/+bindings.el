;;; emacs/dired/+bindings.el -*- lexical-binding: t; -*-

(doom-log "Setting up Dired bindings")
(defvar jg-dired-mode-map (make-keymap))
;; (suppress-keymap jg-dired-mode-map)
;; (evil-make-intercept-map jg-dired-mode-map)

(map! :leader
      :prefix "f"
      "t" #'+jg-list-trash
      )

(defun +jg-dired-group-helper ()
  (interactive)
  (message "Dired Groups: (M)ark, (c)hange, (d)escribe, (o)pen, (e)ncrypt, (s)ort ")
  )

(map! :map jg-dired-mode-map ;; main
      "?"                           #'+jg-dired-group-helper
      :nv "q"                       #'+jg-dired-kill-subdir-or-close-buffer
      :n "!"                        #'dired-do-shell-command
      :n "@"                        #'dired-do-async-shell-command
      :n "#"                        #'+jg-dired-seq-command
      :n "DEL"                      #'dired-kill-subdir
      :n "RET"                      #'dired-find-file
      :n "S"                        #'hydra-dired-quick-sort/body
      :n "."                        #'dired-omit-mode
      :n "n"                        #'evil-ex-search-next
      :n "v"                        #'evil-visual-state
      :n "g"                        #'revert-buffer
      :n "$"                        #'dired-hide-subdir
      ;; "$"                        #'dired-hide-all
      :desc "Expand Subdir"  :n "i" #'+jg-dired-insert-subdir-maybe-recursive
      :desc "Expand Marked"  :n "I" #'+jg-dired-insert-marked-subdir
      :n "y" #'dired-copy-filename-as-kill
      :n "Y" (cmd! (dired-copy-filename-as-kill 0))
      :desc "Fd File"        :n  "sf" #'fd-dired
      )
(map! :map jg-dired-mode-map ;; mark
      :n "t"                                      #'dired-toggle-marks
      :n "m"                                      #'dired-mark
      :n "u"                                      #'dired-unmark
      :n "U"                                      #'dired-unmark-all-marks

      :prefix ("M" . "Mark")
      :desc "flag garbage files"           :n "x" #'dired-flag-garbage-files
      :desc "mark files containing regexp" :n "g" #'dired-mark-files-containing-regexp
      :desc "mark files regexp"            :n "m" #'dired-mark-files-regexp
      :desc "mark symlinks"                :n "s" #'dired-mark-symlinks
      :desc "mark hash duplicates"         :n "h" #'+jg-dired-hash-files
)
(map! :map jg-dired-mode-map ;; movement
      :n "-"                          #'dired-up-directory
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
(map! :map jg-dired-mode-map ;; change
      :desc "Delete"              :n "D" #'+jg-dired-async-trash
      :desc "Touch"               :n "=" #'+jg-dired-touch

      :prefix ("c" . "Change")
      :desc "Replace grep"        :n "G" #'dired-do-find-regexp-and-replace
      :desc "kill"                :n "K" #'dired-do-delete
      :desc "Global Match Rename" :n "R" #'+jg-dired-GLOBAL-do-rename-regexp
      :desc "symlink"             :n "S" #'dired-do-symlink
      :desc "Relative Symlink"    :n "~" #'dired-do-relsymlink

      :desc "compress to"         :n "Z" #'dired-do-compress-to
      :desc "compress"            :n "z" #'dired-do-compress
      :desc "copy"                :n "c" #'dired-do-copy
      :desc "downcase"            :n "d" #'dired-downcase
      :desc "move"                :n "m" #'dired-do-rename
      :desc "new dir"             :n "n" #'dired-create-directory
      :desc "rename"              :n "r" #'+jg-dired-rename
      :desc "upcase"              :n "u" #'dired-upcase
      :desc "Project"             :n "p" #'+jg-dired-cookiecutter
      )
(map! :map jg-dired-mode-map ;; change, specific
      (:prefix (">" . "File Type Specific"))
      (:prefix ("<" . "Make std dirs")
       :desc "Make Tasks Dir" :n "T" (cmd! (dired-create-directory ".tasks"))
       )
      ;; (:prefix ("p" . "pdf"))
      ;; (:prefix ("d" . "pandoc"))
      ;; (:prefix ("b" . "bibtex"))
      )
(map! :map jg-dired-mode-map ;; describe
      :prefix ("d" . "describe")
      :desc "Marked"           :n "m" #'+jg-dired-marked-info
      :desc "Diff"             :n "d" #'dired-diff
      ;;                       :n "d" #'+jg-dired-diff
      :desc "Diff Directories" :n "D" #'ediff-directories
      :desc "Marked Size"      :n "s" #'+jg-dired-dir-size
      :desc "Hide Details"     :n "i" 'dired-hide-details-mode
      :desc "Git Info"         :n "g" #'dired-git-info-mode
      :desc "File Path"        :n "p" (cmd! (dired-copy-filename-as-kill 0))
      :desc "File Name"        :n "n" #'dired-copy-filename-as-kill
      )
(map! :map jg-dired-mode-map ;; open
      :prefix ("o" . "Open")
      :desc "Fundamental"         :n "f" #'+jg-dired-find-literal
      :desc "Other Window"        :n "o" #'dired-find-file-other-window
      :desc "Quicklook"           :n "l" #'+jg-dired-quick-look
      :desc "Find Random Marked"  :n "r" #'+jg-dired-find-random-marked-file
      :desc "Eww"                 :n "e" #'eww-open-file
      )
(map! :map jg-dired-mode-map ;; encryption
      :after epa
      :prefix ("e" . "Encryption")
      :desc "Decrypt"     :n "d" #'epa-dired-do-decrypt
      :desc "Encrypt"     :n "e" #'epa-dired-do-encrypt
      :desc "Sign"        :n "s" #'epa-dired-do-sign
      :desc "Verify"      :n "v" #'epa-dired-do-verify
      :desc "List Keys"   :n "l" #'+jg-dired-epa-list-keys
      :desc "Import Keys" :n "i" #'epa-import-keys

      (:prefix ("K" . "EXPORT")
       :desc "Keys"       :n "k" #'+jg-dired-epa-export-keys
       )
      )

(map! :map jg-dired-mode-map ;; localleader
      :localleader
      )
(map! :map dirvish-mode-map
      :n "b" #'dirvish-goto-bookmark
      :n "z" #'dirvish-show-history
      :n "f" #'dirvish-file-info-menu
      :n "F" #'dirvish-toggle-fullscreen
      :n "l" #'dired-find-file
      :n "h" #'dired-up-directory
      :localleader
      "h" #'dired-omit-mode)

(setq dired-mode-map jg-dired-mode-map)

(provide 'jg-dired-bindings)
