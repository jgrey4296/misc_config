;;; domain-specific/bibtex/+bindings.el -*- lexical-binding: t; -*-
;; Bibtex bindings

(message "Setting up bibtex bindings: %s" (current-time-string))

;;-- bibtex-mode
(map! :map bibtex-mode-map ;; main
      :after bibtex
      :desc "Lock Key"           :n "!"     #'+jg-bibtex-lock-key
      :desc "Insert from Doi"    :n "?"       (cmd! (doi-utils-insert-bibtex-entry-from-doi (read-string "Doi: ")))
      :desc "Clean entry"        :n "TAB"   #'org-ref-clean-bibtex-entry
      :desc "Edit Field"         :n  "\\"   #'+jg-bibtex-edit-field
      :desc "Change Entry Type"  :n "DEL"   #'+jg-bibtex-edit-entry-type
      :desc "Select Entry"       :v "i e"   #'+jg-bibtex-visual-select-entry
      :desc "Open Pdf"           :n "RET"   #'+jg-bibtex-open-pdf
      :desc "Open Folder"        :n "M-RET" #'+jg-bibtex-find-folder
      :desc "Open DWIM"          :n ">"     #'+jg-bibtex-window-dwim
      :desc "Open Dropbox"       :n "<"     #'+jg-bibtex-window-set-dropbox
      )
(map! :map bibtex-mode-map ;; localleader
      :after bibtex
      :localleader
      :desc "Reference"           "1"     (cmd! (browse-url jg-bibtex-reference-url))
      :desc "Open In progress"    "0"     (cmd! (bookmark-jump "in_progress_pdfs" #'switch-to-buffer-other-window))
      :desc "Bibtex Hydra"        "."     #'+jg-bibtex-hydra/body

      :desc "Subcite"             "\\"    #'+jg-bibtex-subcite
      :desc "Build Bibliography"  "B"     #'org-ref-build-full-bibliography
      :desc "Insert from DOI"     "d"     (cmd! (doi-utils-insert-bibtex-entry-from-doi (read-string "DOI: ")))
      :desc "New Entry"           "n"     #'bibtex-entry
      :desc "Get Meta"            "m"     #'+jg-bibtex-meta-retrieval
      :desc "Count Entries"       "C"     #'bibtex-count-entries
      :desc "Refile"              "r"     #'+jg-bibtex-refile-by-year
      :desc "Rename file"         "R"     #'+jg-bibtex-rename-file
      :desc "Scholar Search"      "s"     #'+jg-bibtex-google-scholar
      :desc "Toggle Watchers"     "W"     #'+jg-bibtex-suppress-watchers
      :desc "Update from DOI"     "u"     (cmd! (doi-utils-update-bibtex-entry-from-doi (org-ref-bibtex-entry-doi)))
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
      :desc "Validate"          "V"      #'bibtex-validate
      :desc "Journal<->BookTitle" "TAB"  #'+jg-bibtex-quickswap
      )
(map! :map bibtex-mode-map ;; jump
      :after bibtex
      :localleader
      :prefix ("j" . "Jump")
      :desc "Jump to Pdf"          "p"   #'+jg-bibtex-open-pdf
      :desc "Jump to url"          "u"   #'+jg-bibtex-open-url
      :desc "Jump to doi"          "d"   #'+jg-bibtex-open-doi
      :desc "Jump to Crossref"     "c"   #'+jg-bibtex-goto-crossref-entry
      :desc "Jump to Quicklook"    "l"   #'+jg-bibtex-quicklook-pdf
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

;;-- jg bindings
(map! :map jg-binding-helm-map
      :desc "Bibtex Helm"               "b" #'+jg-bibtex-helm-bibtex
      )

(map! :leader
      :desc "Load Random Bibtex entry"  "o !"   #'+jg-bibtex-load-random
      )

;;-- end jg bindings

;;-- dired
(map! :map dired-mode-map
      :after jg-dired-bindings
      :localleader
      :prefix ("b" . "bibtex")
      :desc "Unify Pdf Locations" "U"   #'+jg-bibtex-dired-unify-pdf-locations
       :desc "Generate Tex for marked Bibtex" "g" #'+jg-bibtex-dired-compile
       :desc "Compile marked tex" "C" #'+jg-bibtex-dired-compile-run
      )

;;-- end dired

;;-- evil-ex
(message "Setting up bibtex evil ex: %s" (current-time-string))
(evil-ex-define-cmd "ci[te]" #'+jg-bibtex-insert-wrapped)

;;-- end evil-ex
