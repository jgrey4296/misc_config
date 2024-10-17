;;; domain-specific/bibtex/+bindings.el -*- lexical-binding: t; -*-
;; Bibtex bindings

(dlog! "Setting up bibtex bindings: %s" (current-time-string))
;; (evil-make-overriding-map jg-bibtex-mode-map)

;;-- bibtex-mode
(map! :map jg-bibtex-mode-map ;; main
      :n "|" #'general-insert-call
      :desc "Lock Key"           :n "!"     #'+jg-bibtex-lock-key
      :desc "Insert from Doi"    :n "I d"    #'+jg-bibtex-insert-entry-from-doi
      :desc "Auto Form"          :n "I F"     #'+jg-bibtex-entry-form
      :desc "Edit Field"         :n  "\\"   #'+jg-bibtex-edit-field
      :desc "Clean entry"        :n "TAB"   #'org-ref-clean-bibtex-entry
      :desc "Change Entry Type"  :n "DEL"   #'+jg-bibtex-edit-entry-type
      :desc "Open DWIM"          :n ">"     #'+jg-bibtex-window-dwim
      :desc "Open Dropbox"       :n "<"     #'+jg-bibtex-window-set-dropbox

      :desc "Select Entry"       :v "i e"   #'+jg-bibtex-visual-select-entry
      )

(map! :map jg-bibtex-mode-map ;; jump bindings
      ;; :n "s j" nil
      (:prefix ("s j" . "bibtex")
      :desc "to Random entry"     :n "r"   #'+jg-bibtex-load-random
      :desc "to Pdf"              :n "p"   #'+jg-bibtex-open-pdf
      :desc "to Folder"           :n "f"   #'+jg-bibtex-open-folder
      :desc "to url"              :n "u"   #'+jg-bibtex-open-url
      :desc "to doi"              :n "d"   #'+jg-bibtex-open-doi
      :desc "to Crossref"         :n "c"   #'+jg-bibtex-goto-crossref-entry
      :desc "to Quicklook"        :n "l"   #'+jg-bibtex-quicklook-pdf
      :desc "to ORCID"            :n "o"   #'+jg-bibtex-lookup-orcid
      :desc "to scholar"          :n "s"   #'+jg-bibtex-google-scholar
      )
      :i "s j" nil
      :i "s" nil
      :i "s" #'self-insert-command
      )

(map! :map jg-bibtex-mode-map ;; groups
      :localleader
      (:prefix ("c" . "Copy"))
      (:prefix ("f" . "Format"))
      (:prefix ("i" . "Insert"))
      (:prefix ("r" . "Refile"))
      (:prefix ("s" . "Sort"))
      (:prefix ("u" . "Update"))
      (:prefix ("v" . "Vars"))
      )

(map! :map jg-bibtex-mode-map ;; localleader
      :localleader
      :desc "Remove Field"        "DEL"     #'+jg-bibtex-remove-field
      :desc "Subcite"             "\\"      #'+jg-bibtex-subcite
      :desc "Build Bibliography"  "B"       #'org-ref-build-full-bibliography
      :desc "Get Meta"            "m"       #'+jg-bibtex-meta-retrieval
      :desc "Count Entries"       "C"       #'bibtex-count-entries
)

(map! :map jg-bibtex-mode-map ;; refile
      :localleader
      :prefix ("r" . "Refile")
       :desc "Refile to Unsourced" "U"     #'+jg-bibtex-refile-to-unsourced
       :desc "Refile"              "r"     #'+jg-bibtex-refile-by-year
       :desc "to Other Window"     "o"     #'+jg-bibtex-refile-to-other-window
       )

(map! :map jg-bibtex-mode-map ;; insert/copy/edit/update/format
      :localleader
      (:prefix ("i" . "Insert")
      :desc "New Entry"           "e"     #'bibtex-entry
      :desc "Auto Form"           "a"     #'+jg-bibtex-entry-form
      :desc "from DOI"            "d"     #'+jg-bibtex-insert-entry-from-doi
      :desc "from PMID"           "p"     (cmd! (pubmed-insert-bibtex-from-pmid (read-string "PMID: ")))
      :desc "Subcite"             "s"     #'+jg-bibtex-subcite
      )
      (:prefix ("c" . "Copy")
      :desc "Copy Entry"         "e"      #'+jg-bibtex-copy-entry
      :desc "Copy Key"           "k"      #'+jg-bibtex-copy-key
      :desc "Copy Title"         "t"      #'+jg-bibtex-copy-title
      :desc "Copy Field"         "f"      #'+jg-bibtex-copy-field
      :desc "Copy into metadata" "m"      #'+jg-bibtex-apply-meta
      )
      (:prefix ("u" . "Update")
       :desc "Update Field"        "f"      #'+jg-bibtex-edit-field
       :desc "Remove Field"        "F"      #'+jg-bibtex-remove-field
       :desc "from DOI"            "u"      #'+jg-bibtex-update-entry
       :desc "filename"            "n"      #'+jg-bibtex-rename-file
       :desc "Lock Key"            "k"      #'+jg-bibtex-lock-key
       :desc "Entry Type"          "t"      #'+jg-bibtex-edit-entry-type
       :desc "Missing fields "     "m"     #'bibtex-entry-update
       :desc "With Newest download"  "d"     #'+jg-bibtex-use-newest-file
       )
      (:prefix ("f" . "Format")
       :desc "Reformat Buffer"       "b"      #'+jg-bibtex-reformat-buffer
       :desc "Format Entry"          "e"      #'org-ref-clean-bibtex-entry
       :desc "Author <-> Editor"     "a"      #'+jg-bibtex-swap-editor-author
       :desc "Journal <-> Booktitle" "j"      #'+jg-bibtex-swap-booktitle-journal
       :desc "Validate"              "v"      #'bibtex-validate
       :desc "Sort"                  "s"      #'+jg-bibtex-cleanup-sort-entry
       :desc "Kill Key"              "k"      #'+jg-bibtex-kill-entry-key
       )
      )

(map! :map jg-bibtex-mode-map ;; sort
      :localleader
      :prefix ("s" . "Sort")
       :desc "Sort Buffer"       "s"      #'bibtex-sort-buffer
       :desc "Sort By Year"      "y"      #'+jg-bibtex-sort-buffer-by-year
       :desc "Sort By Type"      "t"      #'+jg-bibtex-sort-buffer-by-type
      )

(map! :map jg-bibtex-mode-map ;; vars
      :localleader
      ;; TODO search in crossref
      :prefix ("v" . "Vars")
      :desc "Clean Error Move"       "m"     #'+jg-bibtex-clean-error-move-toggle
      :desc "Toggle PDF+Doi Open"    "d"     #'+jg-bibtex-toggle-doi-load
      :desc "Toggle PDF+Url Open"    "u"     #'+jg-bibtex-toggle-url-load
      :desc "Toggle Watchers"        "W"     #'+jg-bibtex-suppress-watchers
      )

;;-- end bibtex-mode

;;-- reftex
(map! :map reftex-mode-map
      "C-c [" nil)

;;-- end reftex

;;-- helm
(map! :map jg-binding-helm-map
      :desc "Bibtex Ivy"                "b" #'ivy-bibtex
      :desc "Bibtex Helm"               "B" #'+jg-bibtex-helm-bibtex
      :desc "Bibtex Local"              "l" #'ivy-bibtex-with-local-bibliography
      :desc "Bibtex Helm Local"         "L" #'helm-bibtex-with-local-bibliography
      )

;;-- end helm

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
(dlog! "Setting up bibtex evil ex: %s" (current-time-string))
(evil-ex-define-cmd "ci[te]" #'+jg-bibtex-insert-wrapped)

;;-- end evil-ex

(map! :map bibtex-style-mode-map
      :n "|" #'general-insert-call
      )

(after! bibtex
  (setq bibtex-mode-map jg-bibtex-mode-map)
  )
