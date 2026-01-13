;;; +bind-changes.el -*- lexical-binding: t; no-byte-compile: t; -*-

(set-keymap-parent jg-dired-change-map jgb-change--nontext-map)

(map! :map jg-dired-change-map
      (:prefix ("f" . "Change Files"))
      (:prefix ("d" . "Change Dir"))
      (:prefix ("l" . "Create Links"))
      (:prefix ("z" . "Zip Files"))
      )

(map! :map jg-dired-change-map ;; change
      :desc "copy"                :n "c" #'dired-do-copy
      :desc "copy-async"          :n "C" #'dired-async-do-copy
      :desc "rename"              :n "r" #'+jg-dired-rename
      :desc "move-async"          :n "m" #'dired-do-rename
      :desc "move-async"          :n "M" #'dired-async-do-rename
      :desc "New Dir"             :n "n" #'dired-create-directory

      (:prefix "z"
       :desc "named compress"         :n "n" #'dired-do-compress-to
       :desc "compress"               :n "z" #'dired-do-compress
       :desc "Zip File List"          :n "l" #'+jg-dired-async-list-zip-files
       :desc "Zip Extract File"       :n "e" #'+jg-dired-extract-from-zip-file
       )

      (:prefix "l"
       :desc "symlink"             :n "s" #'dired-do-symlink
       :desc "Relative Symlink"    :n "r" #'dired-do-relsymlink
       :desc "Hardlink"            :n "h" #'dired-do-hardlink
       )
      )

(map! :map jg-dired-change-map ;; change files
      :prefix "f"
      :desc "Replace grep"        :n "G" #'dired-do-find-regexp-and-replace
      :desc "kill"                :n "K" #'dired-do-delete
      :desc "Global Match Rename" :n "R" #'+jg-dired-GLOBAL-do-rename-regexp

      :desc "Tesseract"           :n "t" #'+jg-dired-tesseract
      :desc "copy"                :n "c" #'dired-async-do-copy
      :desc "downcase"            :n "j" #'dired-downcase
      :desc "upcase"              :n "k" #'dired-upcase
      :desc "move"                :n "m" #'dired-async-do-rename
      :desc "rename"              :n "r" #'+jg-dired-rename
      :desc "Owner"               :n "o" #'dired-do-chown
      :desc "Permissions"         :n "O" #'dired-do-chmod
)

(map! :map jg-dired-change-map ;; change dir
      :prefix "d"
      :desc "Cookiecutter"    :n   "c" #'+jg-dired-cookiecutter
      :desc "New Dir"         :n   "n" #'dired-create-directory
      :desc "New Tasks Dir"   :n   "t" (cmd! (dired-create-directory ".tasks"))
      :desc "New Docs Dir"    :n   "d" (cmd! (dired-create-directory "docs"))
      :desc "New Tests Dir"   :n   "x" (cmd! (dired-create-directory "__tests"))
)

;;; +bind-changes.el ends here
