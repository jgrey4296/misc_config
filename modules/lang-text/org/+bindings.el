;;; main/jg-org/+bindings.el -*- lexical-binding: t; -*-
(dlog! "Setting up general access org bindings: %s" (current-time-string))
(local-load! "util/+org-standard-bindings")

(defvar jg-org-mode-map (make-sparse-keymap))
(evil-make-overriding-map jg-org-mode-map)
(set-keymap-parent jg-org-mode-map jg-org-base-map)

(defvar jg-org-capture-map (make-sparse-keymap))

(map! :leader
      :desc "Insert Timestamp"  "i t"   #'org-time-stamp
      :prefix ("o" . "Org")
      (:prefix ("a" . "Org Agenda")
       :desc "Agenda"                "a"          #'org-agenda
       :desc "Todo list"             "t"          #'org-todo-list
       :desc "Tags search"           "m"          #'org-tags-view
       :desc "View search"           "v"          #'org-search-view
       :desc "List Agenda Files"     "F"          #'+jg-org-list-agenda-files
       :desc "occur-in-agenda-files" "/"          #'org-occur-in-agenda-files
       :desc "agenda-file-to-front"  "f"          #'org-agenda-file-to-front
       :desc "remove-file"           "r"          #'org-remove-file
       :desc "agenda-list"           "l"          #'org-agenda-list
       )
      )

(map! :map jg-org-mode-map
      :n "|" #'general-insert-call
      :desc "Next Link"       :m "] l" #'org-next-link
      :desc "Prev Link"       :m "[ l" #'org-previous-link
      :desc "Forward Heading" :m "] j" #'org-forward-heading-same-level
      :desc "Back Heading"    :m "[ j" #'org-backward-heading-same-level
      :desc "Hide Drawers"    :n "z d" (cmd! (org-cycle-hide-drawers 'org-cycle-hide-drawers))
      :desc "DWIM"            :n "RET" #'+org/dwim-at-point
      ;; :desc "Insert Heading"  :n "M-RET" #'org-insert-heading
      ;; :desc "Insert Item"     :i "M-RET" #'+org/insert-item-below
      :vn "c l" #'org-demote-subtree
      :vn "c h" #'org-promote-subtree
      :vn "c K" #'org-move-subtree-up
      :vn "c J" #'org-move-subtree-down
      :n "TAB"  #'org-sort
      )

(map! :map jg-org-mode-map
      :localleader
      "1" #'org-element-cache-reset
      :desc "New SubHeading"         "DEL" #'org-insert-subheading
      :desc "New Heading"            "RET" #'org-insert-heading
      :desc "Refile"                 "r" #'+org/refile-to-visible
      :desc "Todo"                   "TAB" #'org-todo
      :desc "Lint"                   "L" #'org-lint

      (:prefix ("f". "Format")
       :desc "Clean Org"             "c" #'+jg-org-clean-master
       :desc "Wrap Numbers"          "w" #'+jg-org-wrap-numbers
       :desc "Wrap non-link urls"    "L" #'+jg-org-wrap-non-link-urls
       :desc "Remove Duplicates"     "D" #'+jg-org-remove-duplicate-tweet-entries
       )
      ;; TODO refine this Codeblocks
      (:prefix ("." . "Code Blocks")
       :desc "Edit Codeblock "     "e" #'org-edit-src-code
       :desc "Exec Codeblock"      "E" #'org-babel-execute-src-block
       :desc "Clear Result"        "k" #'org-babel-remove-result
       :desc "Clear All Results"   "K" #'+org/remove-result-blocks
       )
      ;; Links
      (:prefix ("l" . "Links")
       ;; "o"   #'+jg-org-open_link_in_buffer
       ;; "O"   #'+jg-org-open_link_externally
       :desc "Change Link Name"    "n" #'+jg-org-change-link-name
       )
      ;; Insertion
      (:prefix ("i" . "Insert")
       :desc "Insert Heading Trio" "t" #'+jg-org-insert-heading-trio
       :desc "Insert Heading"      "H" #'org-insert-heading
       :desc "Insert Subheading"   "h" #'org-insert-subheading
       :desc "Insert Drawer"       "d" #'org-insert-drawer
       :desc "Insert Structure"    "s" #'org-insert-structure-template
       :desc "Insert Item"         "i" #'org-insert-item
       )
      )

(map! :map org-journal-mode-map
       :n "]f"  #'org-journal-next-entry
       :n "[f"  #'org-journal-previous-entry
       :n "C-n" #'org-journal-next-entry
       :n "C-p" #'org-journal-previous-entry
       )
(map! :map org-journal-search-mode-map
            "C-n" #'org-journal-search-next
            "C-p" #'org-journal-search-previous
            )
(map! :map org-journal-mode-map
      :localleader
      (:prefix "j"
               "c" #'org-journal-new-entry
               "d" #'org-journal-new-date-entry
               "n" #'org-journal-next-entry
               "p" #'org-journal-previous-entry
               )
      (:prefix "s"
               "s" #'org-journal-search
               "f" #'org-journal-search-forever
               "F" #'org-journal-search-future
               "w" #'org-journal-search-calendar-week
               "m" #'org-journal-search-calendar-month
               "y" #'org-journal-search-calendar-year
               )
      )
(map! :map org-journal-search-mode-map
      "n" #'org-journal-search-next
      "p" #'org-journal-search-prev
      )
(map! :map org-unit-test-map
      :localleader
      :prefix "."
      :desc "Run Org Test" "T" #'+jg-org-test-org-file
      )
(map! :map org-src-mode-map
      :n "Q" #'org-edit-src-abort
      :n "q" #'org-edit-src-save
      :n "C-c C-c" #'org-edit-src-save
      :n "C-c C-k" #'org-edit-src-abort
      :localleader
      "q" #'org-edit-src-abort
      "w" #'org-edit-src-save

      )

(map! :map dired-mode-map
      :after jg-dired-bindings
      :desc "Mark Orgs" "Mo" #'+jg-org-dired-select-org
      :desc "Display Selection" "os" #'+jg-org-display-selection

      (:prefix (">o" . "Org")
        "e" #'+jg-org-dired-export
        )

      :localleader
      (:prefix ("K" . "Destructive")
       :desc "Clean Marked" "c"     #'+jg-org-dired-clean
       )
      )

(map! :map jg-org-capture-map
      :n "RET" #'org-capture-finalize
      :n "q"   #'org-capture-kill
      )


(after! (evil-org org)
  (setq org-mode-map jg-org-mode-map
        evil-org-mode-map jg-org-mode-map
        minor-mode-map-alist (assq-delete-all 'evil-org-mode minor-mode-map-alist)
        org-capture-mode-map jg-org-capture-map
        )
  (push (cons 'evil-org-mode jg-org-mode-map) minor-mode-map-alist)
  (evil-make-overriding-map org-src-mode-map)
  )
