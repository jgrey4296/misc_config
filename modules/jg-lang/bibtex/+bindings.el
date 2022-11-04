;;; domain-specific/bibtex/+bindings.el -*- lexical-binding: t; -*-
;; Bibtex bindings

(message "Setting up bibtex bindings: %s" (current-time-string))
(map! :after bibtex
      :map bibtex-mode-map
      :desc "Insert from Doi"    :n "?"       (cmd! (doi-utils-insert-bibtex-entry-from-doi (read-string "Doi: ")))
      :desc "Clean entry"        :n "TAB"     #'org-ref-clean-bibtex-entry
      :desc "Edit Field"         :n  "\\"     #'+jg-bibtex-edit-field
      :desc "Change Entry Type " :n "|"       #'+jg-bibtex-edit-entry-type
      :desc "Change Entry Type"  :n "DEL"     #'+jg-bibtex-edit-entry-type
      :desc "Select Entry"       :v "i e"     #'+jg-bibtex-visual-select-entry
      :desc "Open Pdf"           :n "RET"     #'+jg-bibtex-open-pdf
      :desc "Open Folder"        :n "M-RET"   #'+jg-bibtex-find-folder
      )
(map! :after bibtex
      :map bibtex-mode-map
      :localleader
      :desc "Open Download"       "1"     #'+jg-bibtex-window-set-downloads
      :desc "Open Dropbox"        "2"     #'+jg-bibtex-window-set-dropbox
      :desc "Open In progress"    "0"     (cmd! (bookmark-jump "in_progress_pdfs" #'switch-to-buffer-other-window))
      :desc "Bibtex Hydra"        "."     #'+jg-bibtex-hydra/body
      :desc "Build Bibliography"  "B"     #'org-ref-build-full-bibliography
      :desc "New Entry"           "n"     #'bibtex-entry
      :desc "Get Meta"            "m"     #'+jg-bibtex-meta-retrieval
      :desc "Count Entries"       "C"     #'bibtex-count-entries
      :desc "Refile"              "r"     #'+jg-bibtex-refile-by-year
      :desc "Rename file"         "R"     #'+jg-bibtex-rename-file
      :desc "Scholar Search"      "s"     #'+jg-bibtex-google-scholar
      :desc "Toggle Watchers"     "W"     #'+jg-bibtex-suppress-watchers
      :desc "Update from DOI"     "u"     (cmd! (doi-utils-update-bibtex-entry-from-doi (org-ref-bibtex-entry-doi)))
      (:prefix ("c" . "Copy")
       :desc "Copy Entry"         "e"      #'+jg-bibtex-copy-entry
       :desc "Copy Key"           "k"      #'+jg-bibtex-copy-key
       :desc "Copy Title"         "t"      #'+jg-bibtex-copy-title
       :desc "Copy Field"         "f"      #'+jg-bibtex-copy-field
       :desc "Copy into metadata" "m"      #'+jg-bibtex-apply-meta
       )
      (:prefix ("e" . "Edit")
       :desc "Change Entry Type" "t"      #'+jg-bibtex-edit-entry-type
       :desc "Update Entry"      "U"      #'bibtex-entry-update
       :desc "Update Field"      "f"      #'+jg-bibtex-edit-field
       :desc "Sort Buffer"       "s"      #'bibtex-sort-buffer
       :desc "Validate"          "V"      #'bibtex-validate
       :desc "Journal<->BookTitle" "TAB"  #'+jg-bibtex-quickswap
       )
      (:prefix ("j" . "Jump")
       :desc "Jump to Pdf"          "p"   #'+jg-bibtex-open-pdf
       :desc "Jump to Pdf Folder"   "P"   #'+jg-bibtex-find-folder
       :desc "Jump to url"          "u"   #'+jg-bibtex-open-url
       :desc "Jump to doi"          "d"   #'+jg-bibtex-open-doi
       :desc "Jump to Crossref"     "c"   #'+jg-bibtex-goto-crossref-entry
       :desc "Jump to Quicklook"    "l"   #'+jg-bibtex-quicklook-pdf
       )
      ;; TODO search in crossref
      (:prefix ("v" . "Vars")
       :desc "Clean Error Move"       "m" #'+jg-bibtex-clean-error-move-toggle
       :desc "Toggle PDF+Doi Open"    "d" #'+jg-bibtex-toggle-doi-load
       :desc "Toggle PDF+Url Open"    "u" #'+jg-bibtex-toggle-url-load
       )
      )

;; Reftex binding override
(map! :map reftex-mode-map
      "C-c [" nil)

(map! :map jg-binding-helm-map
      :desc "Bibtex Helm"               "b" #'+jg-bibtex-helm-bibtex
      )

(map! :leader
      :desc "Load Random Bibtex entry"  "o !"   #'+jg-bibtex-load-random
      )

(map! :after dired
      :map dired-mode-map
      :localleader
      :prefix "K"
      :desc "Unify Pdf Locations" "U"   #'+jg-bibtex-dired-unify-pdf-locations
      :desc "Stub Pdfs"           "P"   #'+jg-bibtex-dired-stub-entries
      (:prefix ("b" . "Bibtex")
       :desc "Generate Tex for marked Bibtex" "g" #'+jg-bibtex-dired-compile
       :desc "Compile marked tex" "C" #'+jg-bibtex-dired-compile-run
       )
      )

(message "Setting up bibtex evil ex: %s" (current-time-string))
(evil-ex-define-cmd "ci[te]" #'+jg-bibtex-insert-wrapped)
