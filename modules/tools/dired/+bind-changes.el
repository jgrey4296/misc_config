;;; +bind-changes.el -*- lexical-binding: t; no-byte-compile: t; -*-

(set-keymap-parent jg-dired-change-map jgb-change--nontext-map)

(map! :map jg-dired-change-map
      (:prefix ("f" . "Change Files"))
      (:prefix ("d" . "Change Dir"))
      (:prefix ("l" . "Create Links"))
      (:prefix ("z" . "Zip Files"))
      )

(map! :map jg-dired-change-map ;; change
      :desc "copy"                "c" #'dired-do-copy
      :desc "copy-async"          "C" #'dired-async-do-copy
      :desc "rename"              "r" #'+jg-dired-rename
      :desc "move-async"          "m" #'dired-do-rename
      :desc "move-async"          "M" #'dired-async-do-rename
      :desc "New Dir"             "n" #'dired-create-directory

      (:prefix "z"
       :desc "named compress"         "n" #'dired-do-compress-to
       :desc "compress"               "z" #'dired-do-compress
       :desc "Zip File List"          "l" #'+jg-dired-async-list-zip-files
       :desc "Zip Extract File"       "e" #'+jg-dired-extract-from-zip-file
       )

      (:prefix "l"
       :desc "symlink"             "s" #'dired-do-symlink
       :desc "Relative Symlink"    "r" #'dired-do-relsymlink
       :desc "Hardlink"            "h" #'dired-do-hardlink
       )
      )

(map! :map jg-dired-change-map ;; change files
      :prefix "f"
      :desc "Replace grep"        "G" #'dired-do-find-regexp-and-replace
      :desc "kill"                "K" #'dired-do-delete
      :desc "Global Match Rename" "R" #'+jg-dired-GLOBAL-do-rename-regexp

      :desc "Tesseract"           "t" #'+jg-dired-tesseract
      :desc "copy"                "c" #'dired-async-do-copy
      :desc "downcase"            "j" #'dired-downcase
      :desc "upcase"              "k" #'dired-upcase
      :desc "move"                "m" #'dired-async-do-rename
      :desc "rename"              "r" #'+jg-dired-rename
      :desc "Owner"               "o" #'dired-do-chown
      :desc "Permissions"         "O" #'dired-do-chmod
)

(map! :map jg-dired-change-map ;; change dir
      :prefix "d"
      :desc "Cookiecutter"    "c" #'+jg-dired-cookiecutter
      :desc "New Dir"         "n" #'dired-create-directory
      :desc "New Tasks Dir"   "t" (cmd! (dired-create-directory ".tasks"))
      :desc "New Docs Dir"    "d" (cmd! (dired-create-directory "docs"))
      :desc "New Tests Dir"   "x" (cmd! (dired-create-directory "__tests"))
)

;;; +bind-changes.el ends here
