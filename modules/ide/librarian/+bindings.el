;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Open Url"               "?"       #'librarian-url
      :desc "Dir: Librarian-regular" "y 4"     (cmd! (find-file librarian-regular-loc))
      :desc "Dir: General Insert"    "y 5"     (cmd! (find-file librarian-insert-loc))
      :desc "General Insert"         "i g"     #'librarian-insert-trigger
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
       :i "s" #'self-insert-command
      (:prefix ("s" . "Jump")
       :desc "Browse URL"                :n "u" #'librarian-url
       :desc "Select Link"               :n "," #'link-hint-open-link
       :desc "Librarian Regular"         :n "1" #'librarian-regular-go!

       (:prefix ("k" . "Lookup")
        :desc "Choose Handler"             :n    ";" #'librarian-choose
        :desc "Assignments"                :n    "a" #'librarian-assignments
        :desc "Type definition"            :n    "t" #'librarian-type-definition
        :desc "References"                 :n    "r" #'librarian-references
        :desc "Definition"                 :n    "d" #'librarian-definition
        :desc "Declaration"                :n    "D" #'librarian-declaration
        :desc "Implementations"            :n    "i" #'librarian-implementations
        :desc "Documentation"              :n    "k" #'librarian-documentation
        :desc "Look up in local docsets"   :n    "K" #'librarian-docsets-consult

        :desc "Look up online (w/ prompt)" :n    "1" #'librarian-online-select
        :desc "Look up online"             :n    "2" #'librarian-online

        :desc "Dictionary"                 :n    "w" #'librarian-words-definition
        :desc "Thesaurus"                  :n    "W" #'librarian-words-synonyms

        :desc "Debug"                      :n    "?" #'librarian-debug
        :desc "Install Docset"             :n    "0" #'librarian-docsets-install
        :desc "Local Variables"            :n    "L" #'librarian-buffer-locals
        )
       )
      )

(map! :map librarian-mode-map
      :desc "ispell-word"               :n "c w s"    #'ispell-word
      :desc "Ivy Spell"                 :n "c w S"    #'flyspell-correct-wrapper
      :desc "add word to dict"          :n "c w a"    #'+spell/add-word
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
