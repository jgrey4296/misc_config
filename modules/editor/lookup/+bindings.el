;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Open Url"              "?"   #'browse-select-goto-url

      (:prefix "g"
       :desc "Docs: Git Manual"      "1" (cmd! (browse-url jg-browse-github-url))
       )
      )

(map! :map jg-binding-jump-map
      :desc "Browse URL"           "u" #'browse-select-goto-url
      :desc "Lookup Regular"        "1" #'lookup-regular-go

      (:prefix ("k" . "Docs")
       :desc "Type definition"       "t" #'+lookup/type-definition
       :desc "References"            "r" #'+lookup/references
       :desc "Definition"            "d" #'+lookup/definition
       :desc "Implementations"       "i" #'+lookup/implementations
       :desc "Find other file"       "o" #'projectile-toggle-between-implementation-and-test
       :desc "Documentation"         "k" #'+lookup/documentation
       :desc "Word(net)"                 "w" #'helm-wordnet-suggest
       :desc "Word(nut)"                 "W" #'wordnut-search
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
