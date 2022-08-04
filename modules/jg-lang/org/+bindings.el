;;; main/jg-org/+bindings.el -*- lexical-binding: t; -*-
(message "Setting up general access org bindings: %s" (current-time-string))
(map! :after jg-leader-bindings-loaded
      :leader
      "i t"    #'org-time-stamp
      "j c"    #'org-goto-calendar
      (:prefix "o"
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
      )

(map! :after dired
      :map dired-mode-map
      (:prefix "%"
       :desc "Mark Orgs" :n "o"     #'+jg-org-dired-select-org
       )
      :localleader
      (:prefix ("m" . "Mark")
       :desc "Mark Orgs" "o" #'+jg-org-dired-select-org
       )
      (:prefix ("f" . "Find")
       :desc "Display Selection" "s" #'+jg-org-display-selection
       )
      (:prefix ("K" . "Destructive")
       :desc "Clean Marked" "c"     #'+jg-org-dired-clean
       :desc "Chop File Names" "C"  #'+jg-org-chop-long-files-from-dired
       :desc "Quick Compress" "Z"   #'+jg-org-quick-compress-orgs
       :desc "Fix Org Links"  "L"   #'+jg-org-dired-fix-org-links
       :desc "Mark as Twitter"  "T" #'+jg-org-dired-add-twitter-prop
       )
      )

(map! :after org-agenda
      :map org-agenda-mode-map
      :localleader
      (:prefix ("d" . "Date/time"))
      (:prefix ("c" . "Clock"))
      (:prefix ("p" . "Priority")))

;;(define-key (evil-get-auxiliary-keymap org-mode-map 'insert) (kbd "<SPC>") nil)

(message "Setting up org personal bindings: %s" (current-time-string))
;; Wiping old
;; Personal
;; <leaderless>
(map! :after org
      :map org-mode-map
      :n "] h" nil
      :n "[ h" nil
      :desc "Next Link"       :n "] l" #'org-next-link
      :desc "Prev Link"       :n "[ l" #'org-previous-link
      :desc "Forward Heading" :n "] j" #'org-forward-heading-same-level
      :desc "Back Heading"    :n "[ j" #'org-backward-heading-same-level
      :desc "Headings Helm"   :n "g h" #'helm-org-in-buffer-headings
      )
;; <leader>
(map! :after org
      :map org-mode-map
      :leader
      :desc "Toggle Links" "t l"   'org-toggle-link-display)
;; <localleader>
(map! :after org
      :map org-mode-map
      :localleader
      :desc "Refile" "R" #'+jg-org-refile-subtree
      :desc "Todo"   "TAB" #'org-todo
      :desc "Docs: Org"               "1" (cmd! (+jg-misc-browse-url "https://orgmode.org/manual/"))
      :desc "Docs: Babel Snippets"   "2" (cmd! (+jg-misc-browse-url "https://orgmode.org/worg/org-contrib/babel/intro.html"))

      (:prefix ("f". "Format")
       :desc "Fix Drawers"        "d"  #'+jg-org-fix-properties-drawers
       :desc "Clean Org"          "c"  #'+jg-org-clean-master
       :desc "Wrap Numbers"        "w" #'+jg-org-wrap-numbers
       :desc "Wrap non-link urls"  "L" #'+jg-org-wrap-non-link-urls
       :desc "Remove Duplicates"   "D" #'+jg-org-remove-duplicate-tweet-entries
       :desc "Set as Twitter Buffer" "t" #'+jg-org-add-twitter-property
       )
      ;; TODO refine this Codeblocks
      (:prefix ("." . "Code Blocks")
       :desc "Edit Codeblock " "e"     #'org-edit-src-code
       :desc "Exec Codeblock"  "E"     #'org-babel-execute-src-block)
      ;; Links
      (:prefix ("l" . "Links")
       ;; "o"   #'+jg-org-open_link_in_buffer
       ;; "O"   #'+jg-org-open_link_externally
       :desc "Change Link Name" "n"      #'+jg-org-change-link-name
       )
      ;; Insertion
      (:prefix ("i" . "Insert")
       :desc "Insert Heading Trio" "t" #'+jg-org-insert-heading-trio
       :desc "Insert Subheading" "h"   #'org-insert-subheading
       :desc "Insert Drawer" "d"       #'org-insert-drawer)

      )

(map! :after evil-org
      :map evil-org-mode-map
      :n "za"  nil
      :n "zA"  nil
      :n "zc"  nil
      :n "zC"  nil
      :n "zm"  nil
      :n "zM"  nil
      :n "zo"  nil
      :n "zR"  nil

      :m "] h"  nil
      :m "[ h"  nil
      )

(map! :after graphviz-dot-mode
      :map graphviz-dot-mode-map
      :localleader
      :desc "Documentation" "1" (cmd! (+jg-misc-browse-url "https://graphviz.org/doc/info/lang.html"))
      )
