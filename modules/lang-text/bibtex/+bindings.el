;;; domain-specific/bibtex/+bindings.el -*- lexical-binding: t; -*-
;; Bibtex bindings

(doom-log "Setting up bibtex bindings: %s" (current-time-string))


;;-- bibtex-mode
(map! :map bibtex-mode-map ;; main
      :after bibtex
      :n "|" #'general-insert-call
      :desc "Lock Key"           :n "!"     #'+jg-bibtex-lock-key
      :desc "Insert from Doi"    :n "Id"    #'+jg-bibtex-insert-entry-from-doi
      :desc "Insert from PMID"   :n "Ip"    (cmd! (pubmed-insert-bibtex-from-pmid (read-string "PMID: ")))
      :desc "Clean entry"        :n "TAB"   #'org-ref-clean-bibtex-entry
      :desc "Edit Field"         :n  "\\"   #'+jg-bibtex-edit-field
      :desc "Change Entry Type"  :n "DEL"   #'+jg-bibtex-edit-entry-type
      :desc "Select Entry"       :v "i e"   #'+jg-bibtex-visual-select-entry
      :desc "Open Pdf"           :n "RET"   #'+jg-bibtex-open-pdf
      :desc "Open Folder"        :n "M-RET" #'+jg-bibtex-open-folder
      :desc "Open DWIM"          :n ">"     #'+jg-bibtex-window-dwim
      :desc "Open Dropbox"       :n "<"     #'+jg-bibtex-window-set-dropbox
      )
(map! :map bibtex-mode-map ;; localleader
      :after bibtex
      :localleader
      :desc "Reformat Buffer"     "TAB"   #'+jg-bibtex-reformat-buffer
      :desc "Author <-> Editor"   "0"     #'+jg-bibtex-swap-editor-author
      :desc "Journal <-> Booktitle" "9"   #'+jg-bibtex-swap-booktitle-journal


      :desc "Subcite"             "\\"    #'+jg-bibtex-subcite
      :desc "Build Bibliography"  "B"     #'org-ref-build-full-bibliography
      :desc "Insert from DOI"     "d"     #'+jg-bibtex-insert-entry-from-doi
      :desc "Insert from PMID"    "p"     (cmd! (pubmed-insert-bibtex-from-pmid (read-string "PMID: ")))
      :desc "New Entry"           "n"     #'bibtex-entry
      :desc "Get Meta"            "m"     #'+jg-bibtex-meta-retrieval
      :desc "Lookup ORCID"        "o"     #'+jg-bibtex-lookup-orcid
      :desc "Count Entries"       "C"     #'bibtex-count-entries
      :desc "Refile"              "r"     #'+jg-bibtex-refile-by-year
      :desc "Rename file"         "R"     #'+jg-bibtex-rename-file
      :desc "Refile to Unsourced" "U"     #'+jg-bibtex-refile-to-unsourced
      :desc "Scholar Search"      "s"     #'+jg-bibtex-google-scholar
      :desc "Toggle Watchers"     "W"     #'+jg-bibtex-suppress-watchers
      :desc "Update from DOI"     "u"     #'+jg-bibtex-update-entry
      )
(map! :map bibtex-mode-map ;; copy
      :after bibtex
      :localleader
      :prefix ("c" . "Copy")
      :desc "Copy Entry"         "e"      #'+jg-bibtex-copy-entry
      :desc "Copy Key"           "k"      #'+jg-bibtex-copy-key
      :desc "Copy Title"         "t"      #'+jg-bibtex-copy-title
      :desc "Copy Field"         "f"      #'+jg-bibtex-copy-field
      :desc "Copy into metadata" "m"      #'+jg-bibtex-apply-meta
      )
(map! :map bibtex-mode-map ;; edit
      :after bibtex
      :localleader
      :prefix ("e" . "Edit")
      :desc "Change Entry Type" "t"      #'+jg-bibtex-edit-entry-type
      :desc "Update Entry"      "U"      #'bibtex-entry-update
      :desc "Update Field"      "f"      #'+jg-bibtex-edit-field
      :desc "Sort Buffer"       "s"      #'bibtex-sort-buffer
      :desc "Sort By Year"      "y"      #'+jg-bibtex-sort-buffer-by-year
      :desc "Validate"          "V"      #'bibtex-validate
      :desc "Journal<->BookTitle" "TAB"  #'+jg-bibtex-quickswap
      )
(map! :map bibtex-mode-map ;; jump
      :after bibtex
      :i "s" #'self-insert-command
      :prefix ("s j" . "bibtex")
      :desc "Load Random Bibtex entry" :n "r"   #'+jg-bibtex-load-random
      :desc "Jump to Pdf"              :n "p"   #'+jg-bibtex-open-pdf
      :desc "Jump to url"              :n "u"   #'+jg-bibtex-open-url
      :desc "Jump to doi"              :n "d"   #'+jg-bibtex-open-doi
      :desc "Jump to Crossref"         :n "c"   #'+jg-bibtex-goto-crossref-entry
      :desc "Jump to Quicklook"        :n "l"   #'+jg-bibtex-quicklook-pdf
      )
(map! :map bibtex-mode-map ;; vars
      :after bibtex
      :localleader
      ;; TODO search in crossref
      :prefix ("v" . "Vars")
      :desc "Clean Error Move"       "m" #'+jg-bibtex-clean-error-move-toggle
      :desc "Toggle PDF+Doi Open"    "d" #'+jg-bibtex-toggle-doi-load
      :desc "Toggle PDF+Url Open"    "u" #'+jg-bibtex-toggle-url-load
      )

;;-- end bibtex-mode

;;-- reftex
(map! :map reftex-mode-map
      "C-c [" nil)

;;-- end reftex

(map! :map jg-binding-helm-map
      :desc "Bibtex Ivy"                "b" #'ivy-bibtex
      :desc "Bibtex Helm"               "B" #'+jg-bibtex-helm-bibtex
      :desc "Bibtex Local"              "l" #'ivy-bibtex-with-local-bibliography
      :desc "Bibtex Helm Local"         "L" #'helm-bibtex-with-local-bibliography
      )

;;-- dired
(map! :map dired-mode-map
      :after jg-dired-bindings
      :prefix ("> b" . "bibtex")
      :desc "Generate Tex for marked Bibtex" "g" #'+jg-bibtex-dired-generate-tex
      :desc "Compile marked tex"             "C" #'+jg-bibtex-dired-compile-run
      :desc "Check pdf existence"            "?" #'+jg-bibtex-dired-check-pdfs
      )

;;-- end dired

;;-- evil-ex
(doom-log "Setting up bibtex evil ex: %s" (current-time-string))
(evil-ex-define-cmd "ci[te]" #'+jg-bibtex-insert-wrapped)

;;-- end evil-ex

(map! :map bibtex-style-mode-map
      :n "|" #'general-insert-call
      )
