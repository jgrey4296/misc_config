;;; domain-specific/bibtex/+bindings.el -*- lexical-binding: t; -*-
;; Bibtex bindings

(message "Setting up bibtex bindings: %s" (current-time-string))
(map! :map bibtex-mode-map
      :desc "Clean entry"        :n "C-c C-c" #'+jg-bibtex-clean-entry
      :desc "Clean entry"        :n "TAB"     #'+jg-bibtex-clean-entry
      :desc "Edit Field"         :n  "\\"     #'+jg-bibtex-edit-field
      :desc "Change Entry Type " :n "|"       #'+jg-bibtex-edit-entry-type
      :desc "Change Entry Type"  :n "DEL"     #'+jg-bibtex-edit-entry-type
      :desc "Select Entry"       :v "i e"     #'+jg-bibtex-visual-select-entry
      :desc "Open Pdf"           :n "RET"     #'+jg-bibtex-open-pdf
      :desc "Open Folder"        :n "M-RET"   #'+jg-bibtex-find-folder
      )
(map! :map bibtex-mode-map
      :localleader
      :desc "Bibtex Hydra"        "." #'+jg-bibtex-hydra/body
      :desc "Build Bibliography"  "B" #'org-ref-build-full-bibliography
      :desc "New Entry"           "n" #'org-ref-bibtex-new-entry/body
      :desc "Count Entries"       "C" #'bibtex-count-entries
      :desc "Refile"              "r" #'+jg-bibtex-refile-by-year
      :desc "Rename file"         "R" #'+jg-bibtex-rename-file
      :desc "Scholar Search"      "s" #'+jg-bibtex-google-scholar
      :desc "Toggle Watchers"     "W" #'+jg-bibtex-suppress-watchers
      (:prefix ("c" . "Copy")
       :desc "Copy Entry"        "e"    #'+jg-bibtex-copy-entry
       :desc "Copy Key"          "k"    #'+jg-bibtex-copy-key
       :desc "Copy Title"        "t"    #'+jg-bibtex-copy-title
       :desc "Copy Field"        "f"    #'+jg-bibtex-copy-field
       )
      (:prefix ("e" . "Edit")
       :desc "Change Entry Type" "t"  #'+jg-bibtex-edit-entry-type
       :desc "Update Entry"      "U" #'bibtex-entry-update
       :desc "Update Field"      "f" #'+jg-bibtex-edit-field
       :desc "Sort Buffer"       "s" #'bibtex-sort-buffer
       :desc "Validate"          "V" #'bibtex-validate
       )
      (:prefix ("j" . "Jump")
       :desc "Jump to Pdf"          "p" #'+jg-bibtex-open-pdf
       :desc "Jump to Pdf Folder"   "P" #'+jg-bibtex-find-folder
       :desc "Jump to url"          "u" #'+jg-bibtex-open-url
       :desc "Jump to doi"          "d" #'+jg-bibtex-open-doi
       :desc "Jump to Crossref"     "c" #'+jg-bibtex-goto-crossref-entry
       :desc "Jump to Quicklook"    "l" #'+jg-bibtex-quicklook-pdf
       )
      ;; TODO search in crossref
      (:prefix ("v" . "Vars")
       :desc "Clean Error Move"    "m" #'+jg-bibtex-clean-error-move-toggle
       :desc "Toggle PDF+Doi Open"    "d" #'+jg-bibtex-toggle-doi-load
       :desc "Toggle PDF+Url Open"    "u" #'+jg-bibtex-toggle-url-load
       )
      )

;; Reftex binding override
(map! :map reftex-mode-map
      "C-c [" nil)

(message "Setting up general bibtex bindings: %s" (current-time-string))
(map! :leader
      :desc "Bibtex Helm"               "o h b" #'+jg-bibtex-helm-bibtex
      :desc "Load Random Bibtex entry"  "o !"   #'+jg-bibtex-load-random)


(after! evil-ex
  (message "Setting up bibtex evil ex: %s" (current-time-string))
  (evil-ex-define-cmd "ci[te]" #'+jg-bibtex-insert-wrapped)
  )

(map! :after dired
      :map dired-mode-map
      :localleader
      :prefix "K"
      :desc "Unify Pdf Locations" "U"   #'+jg-bibtex-dired-unify-pdf-locations
      :desc "Stub Pdfs"           "P"   #'+jg-bibtex-dired-stub-entries
      )
