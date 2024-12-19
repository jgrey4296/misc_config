;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Open Url"               "?"       #'librarian-url
      :desc "Local Variables"        "b l"     #'librarian-buffer-locals
      :desc "Dir: Librarian-regular" "y 4"     (cmd! (find-file librarian-regular-loc))
      :desc "Dir: General Insert"    "y 5"     (cmd! (find-file librarian-insert-loc))
      :desc "General Insert"         "i g"     #'librarian-insert-trigger

      (:prefix "s"
       ;; :desc "Look up in all docsets"       "K" #'+lookup/in-all-docsets
       :desc "Look up in local docsets"     "k" #'librarian-docsets-consult
       :desc "Look up online (w/ prompt)"   "O" #'librarian-online-select
       :desc "Look up online"               "o" #'librarian-online
       )
      )

(map! :map jg-help-map
      :after jg-help-bindings
      :desc "Man"                         "d m"  #'librarian-man
      :desc "Emacs Version Config"        "p v"  #'librarian-system-config
      :desc "Describe Class"              "c"    #'librarian-describe-class
      :prefix ("e" . "Edit")
      :desc "Bindings"  "b" #'librarian-configs--edit-bindings
      :desc "Vars"      "v" #'librarian-configs--edit-vars
      :desc "Config"    "c" #'librarian-configs--edit-config
      :desc "Spec-Defs" "s" #'librarian-configs--edit-spec-defs
      )

(map! :map librarian-mode-map
      (:prefix ("s" . "Jump")
       :desc "Browse URL"                :n "u" #'librarian-url
       :desc "Librarian Regular"         :n "1" #'librarian-regular-go!
       (:prefix ("k" . "Documentation")
        :desc "Choose Handler"             :n    ";" #'librarian-choose
        :desc "Assignments"                :n    "a" #'librarian-assignments
        :desc "Type definition"            :n    "t" #'librarian-type-definition
        :desc "References"                 :n    "r" #'librarian-references
        :desc "Definition"                 :n    "d" #'librarian-definition
        :desc "Declaration"                :n    "D" #'librarian-declaration
        :desc "Implementations"            :n    "i" #'librarian-implementations
        :desc "Documentation"              :n    "k" #'librarian-documentation
        :desc "Look up in local docsets"   :n    "K" #'librarian-docsets-consult

        :desc "Word(net)"                  :n    "w" #'helm-wordnet-suggest
        :desc "Word(nut)"                  :n    "W" #'wordnut-search

        :desc "Look up online (w/ prompt)" :n    "1" #'librarian-online-select
        :desc "Look up online"             :n    "2" #'librarian-online

        :desc "Dictionary"                 :n    "q" #'librarian-words-definition
        :desc "Thesaurus"                  :n    "Q" #'librarian-words-synonyms

        :desc "Debug"                      :n    "?" #'librarian-debug
        :desc "Install Docset"             :n    "0" #'librarian-docset-install
        )
       )
      :i "s" #'self-insert-command
      )

(map! :map eww-mode-map
      :n "=" 'eww-copy-page-url
      :n "?" 'eww-browse-with-external-browser
      :desc "Render Buffer" :v "R" #'shr-render-buffer
      :localleader
      :desc "Render Buffer" "r" #'shr-render-buffer
      )

(map! :map jg-binding-normal-state-map
      :desc "Lookup"           "K"   #'librarian-documentation
      :desc "General Insert"   "I |" #'librarian-insert-trigger
      )

(map! :map snippet-mode-map
      :n "|" #'librarian-insert-trigger
      )
