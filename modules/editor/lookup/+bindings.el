;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Open Url"              "?"   #'browse-select-goto-url

      (:prefix "s"
      :desc "Look up in all docsets"       "K" #'+lookup/in-all-docsets
      :desc "Look up in local docsets"     "k" #'+lookup/in-docsets
      :desc "Look up online (w/ prompt)"   "O" #'+lookup/online-select
      :desc "Look up online"               "o" #'+lookup/online
      )
      (:prefix "g"
       :desc "Docs: Git Manual"      "1" (cmd! (browse-url jg-browse-github-url))
       )
      )

(map! :map jg-binding-jump-map
      :desc "Browse URL"           "u" #'browse-select-goto-url
      :desc "Lookup Regular"        "1" #'lookup-regular-go

      (:prefix ("k" . "Docs")
       :desc "Assignments"                  "a" #'+lookup/assignments
       :desc "Type definition"              "t" #'+lookup/type-definition
       :desc "References"                   "r" #'+lookup/references
       :desc "Definition"                   "d" #'+lookup/definition
       :desc "Implementations"              "i" #'+lookup/implementations
       :desc "Documentation"                "k" #'+lookup/documentation
       :desc "Look up in local docsets"     "K" #'+lookup/in-docsets

       :desc "Word(net)"                    "w" #'helm-wordnet-suggest
       :desc "Word(nut)"                    "W" #'wordnut-search

       :desc "Look up online (w/ prompt)"   "1" #'+lookup/online-select
       :desc "Look up online"               "2" #'+lookup/online
       :desc "All Docsets"                  "3" #'+lookup/in-all-docsets

       :desc "Dictionary"                   "q" #'+lookup/dictionary-definition
       :desc "Thesaurus"                    "Q" #'+lookup/synonyms

       :desc "Debug"   "?" #'+jg-lookup-debug-settings
       )
      )

(map! :map eww-mode-map
      :n "=" 'eww-copy-page-url
      :n "?" 'eww-browse-with-external-browser
      )

(map! :map jg-binding-normal-state-map
      :desc "Lookup" "K" #'+lookup/documentation
      )

;; The lookup commands are superior, and will consult xref if there are no
;; better backends available.
;; (global-set-key [remap xref-find-definitions] #'+lookup/definition)
;; (global-set-key [remap xref-find-references]  #'+lookup/references)
