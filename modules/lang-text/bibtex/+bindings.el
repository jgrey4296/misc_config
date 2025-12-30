;;; domain-specific/bibtex/+bindings.el -*- lexical-binding: t; -*-
;; Bibtex bindings

(dlog! "Setting up bibtex bindings: %s" (current-time-string))
;; (evil-make-overriding-map jg-bibtex-mode-map)

;;-- bibtex-mode
(map! :map jg-bibtex-mode-map ;; main
      :n "=" (cmd! (org-bibtex-read-buffer (current-buffer)))
      :n "|" nil
      :desc "Lock Key"           :n "!"       #'librarian--biblio-edit-lock-key
      :desc "Insert from Doi"    :n "I d"     #'librarian-biblio-create-from-doi
      :desc "Auto Form"          :n "I F"     #'+jg-bibtex-entry-form
      :desc "Edit Field"         :n  "\\"     #'+jg-bibtex-edit-field
      :desc "Clean entry"        :n "TAB"     #'org-ref-clean-bibtex-entry
      :desc "Change Entry Type"  :n "DEL"     #'librarian--biblio-edit-entry-type
      :desc "Open DWIM"          :n ">"       #'librarian--biblio-edit-window-dwim
      :desc "Open Dropbox"       :n "<"       #'librarian--biblio-edit-window-set-dropbox

      :desc "Select Entry"       :v "i e"     #'librarian--biblio-edit-visual-select-entry

      :n "[" #'evil-backward-section-begin
      :n "]" #'evil-forward-section-begin
      )

(map! :map jg-bibtex-mode-map ;; jump bindings
      ;; :n "s j" nil
      (:prefix "s j"
      :desc "to Random entry"     :n "r"   #'librarian--biblio-edit-load-random
      :desc "to Pdf"              :n "p"   #'librarian--biblio-edit-open-pdf
      :desc "to Folder"           :n "f"   #'librarian--biblio-edit-open-folder
      :desc "to url"              :n "u"   #'librarian--biblio-edit-open-url
      :desc "to doi"              :n "d"   #'librarian--biblio-edit-open-doi
      :desc "to Crossref"         :n "c"   #'librarian--biblio-edit-goto-crossref-entry
      :desc "to Quicklook"        :n "l"   #'librarian--biblio-edit-quicklook-pdf
      :desc "to ORCID"            :n "o"   #'librarian--biblio-edit-lookup-orcid
      :desc "to scholar"          :n "s"   #'librarian--biblio-edit-google-scholar
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
      :desc "Remove Field"          "DEL"     #'+jg-bibtex-remove-field
      :desc "Subcite"               "\\"      #'librarian--biblio-edit-subcite
      :desc "Build Bibliography"    "B"       #'org-ref-build-full-bibliography
      :desc "Get Meta"              "m"       #'librarian-biblio-get-meta
      :desc "Get Url Meta"          "n"       #'jg-bibtex-url-meta
      :desc "Get Url Raw "          "N"       #'jg-bibtex-url-raw
      :desc "Count Entries"         "C"       #'bibtex-count-entries
      :desc "First Untagged Entry"  "t"       #'+jg-bibtex-first-entry-with-no-tags
      :desc "Extract Pages"         "E"       #'+jg-bibtex-extract-pages
)

(map! :map jg-bibtex-mode-map ;; refile
      :localleader
      :prefix ("r" . "Refile")
      :desc "Refile to Unsourced" "U"     #'librarian-biblio-refile-to-unsourced
      :desc "Refile"              "r"     #'librarian-biblio-refile-to-canonical
      :desc "to Other Window"     "o"     #'librarian-biblio-refile-to-other-window
      )

(map! :map jg-bibtex-mode-map ;; insert/copy/edit/update/format
      :localleader
      (:prefix ("i" . "Insert")
      :desc "New Entry"           "e"     #'bibtex-entry
      :desc "Auto Form"           "a"     #'+jg-bibtex-entry-form
      :desc "from DOI"            "d"     #'librarian-biblio-create-from-doi
      :desc "from url"            "u"     #'+jg-bibtex-create-from-url
      :desc "from PMID"           "p"     (cmd! (pubmed-insert-bibtex-from-pmid (read-string "PMID: ")))
      :desc "Subcite"             "s"     #'librarian--biblio-edit-subcite
      )
      (:prefix ("c" . "Copy")
      :desc "Copy Entry"         "e"      #'librarian--biblio-edit-copy-entry
      :desc "Copy Key"           "k"      #'librarian--biblio-edit-copy-key
      :desc "Copy Title"         "t"      #'librarian--biblio-edit-copy-title
      :desc "Copy Field"         "f"      #'librarian--biblio-edit-copy-field
      :desc "Copy into metadata" "m"      #'librarian-biblio-apply-meta
      )
      (:prefix ("u" . "Update")
       :desc "Update Field"        "f"      #'+jg-bibtex-edit-field
       :desc "Remove Field"        "F"      #'+jg-bibtex-remove-field
       :desc "from DOI"            "u"      #'librarian-biblio-update-entry-from-doi
       :desc "filename"            "n"      #'librarian--biblio-edit-rename-file
       :desc "Lock Key"            "k"      #'librarian--biblio-edit-lock-key
       :desc "Entry Type"          "t"      #'+jg-bibtex-edit-entry-type
       :desc "Missing fields "     "m"      #'bibtex-entry-update
       :desc "With Newest download"  "d"    #'librarian--biblio-edit-use-newest-file
       )
      (:prefix ("f" . "Format")
       :desc "Reformat Buffer"       "b"      #'librarian--biblio-clean-reformat-buffer
       :desc "Format Entry"          "e"      #'org-ref-clean-bibtex-entry
       :desc "Author <-> Editor"     "a"      #'librarian--biblio-edit-swap-editor-author
       :desc "Journal <-> Booktitle" "j"      #'librarian--biblio-edit-swap-booktitle-journal
       :desc "Validate"              "v"      #'bibtex-validate
       :desc "Sort"                  "s"      #'librarian--biblio-clean-sort-entry
       :desc "Kill Key"              "k"      #'librarian--biblio-edit-kill-entry-key
       )
      )

(map! :map jg-bibtex-mode-map ;; sort
      :localleader
      :prefix ("s" . "Sort")
       :desc "Sort Buffer"       "s"      #'bibtex-sort-buffer
       :desc "Sort By Year"      "y"      #'librarian--biblio-edit-sort-buffer-by-year
       :desc "Sort By Type"      "t"      #'librarian--biblio-edit-sort-buffer-by-type
      )

(map! :map jg-bibtex-mode-map ;; vars
      :localleader
      ;; TODO search in crossref
      :prefix ("v" . "Vars")
      :desc "Clean Error Move"       "m"     #'librarian--biblio-clean-error-move-toggle
      :desc "Toggle PDF+Doi Open"    "d"     #'librarian--biblio-edit-toggle-doi-load
      :desc "Toggle PDF+Url Open"    "u"     #'librarian--biblio-edit-toggle-url-load
      :desc "Toggle Watchers"        "W"     #'+jg-bibtex-suppress-watchers
      )

;;-- end bibtex-mode

;;-- reftex
(map! :map reftex-mode-map
      )

;;-- end reftex

;;-- helm
(map! :map jg-binding-helm-map
      :desc "Bibtex Ivy"                "b" #'ivy-bibtex
      :desc "Bibtex Helm"               "B" #'+jg-bibtex-helm-bibtex
      :desc "Bibtex Local"              "l" #'ivy-bibtex-with-local-bibliography
      :desc "Bibtex Helm Local"         "L" #'helm-bibtex-with-local-bibliography
      :desc "Rebuild Bib Library"       "1" #'librarian--biblio-build-list
      )

;;-- end helm

;;-- dired
(map! :map jg-dired-mode-map
      :after jg-dired-bindings
      :prefix ("c f b" . "bibtex")
      :desc "Generate Tex for marked Bibtex" "g" #'+jg-bibtex-dired-generate-tex
      :desc "Compile marked tex"             "C" #'+jg-bibtex-dired-compile-run
      :desc "Check pdf existence"            "?" #'+jg-bibtex-dired-check-pdfs
      )

;;-- end dired

;;-- evil-ex
(dlog! "Setting up bibtex evil ex: %s" (current-time-string))
(evil-ex-define-cmd "ci[te]" #'+jg-bibtex-insert-wrapped)

;;-- end evil-ex

(after! bibtex
  (setq bibtex-mode-map jg-bibtex-mode-map)
  )
