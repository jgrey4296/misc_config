;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Open Url"              "?"   #'+jg-browse-url
      :desc "Twitter"               "8" (cmd! (browse-url jg-browse-twitter-url))

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


(map! :map (sh-mode-map shell-mode-map)
      :after shell
      :localleader
      :desc "Docs: Brew"  "1" (cmd! (browse-url "https://brew.sh/"))
      :desc "Docs: Awk"   "2" (cmd! (browse-url "https://www.gnu.org/software/gawk/manual/gawk.html"))
      )