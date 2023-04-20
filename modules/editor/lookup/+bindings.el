;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Open Url"              "?"   #'+jg-browse-url
      :desc "Lookup Regular"        "m 1" #'lookup-regular-go

      (:prefix "g"
       :desc "Docs: Git Manual"      "1" (cmd! (browse-url jg-browse-github-url))
       )
      )

(map! :map jg-binding-jump-map
      :desc "Browse URL"           "u" #'+jg-browse-url
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
