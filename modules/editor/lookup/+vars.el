;;; +vars.el -*- lexical-binding: t; -*-


(setq browse-select-default-prog "firefox"
      browse-url-browser-function 'browse-select-default
      browse-url-default-handlers nil
      lookup-regular-location (expand-file-name "templates/lookup-regular" doom-user-dir)
      )

(setq xref-show-definitions-function #'ivy-xref-show-defs
      xref-show-xrefs-function       #'ivy-xref-show-xrefs)

;;-- specs
(spec-handling-add! lookup-url
                    '(defaults
                      ("DuckDuckGo"         +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
                      ("Github"             "https://github.com/search?ref=simplesearch&q=%s")
                      ("DevDocs.io"         "https://devdocs.io/#q=%s")
                      ("StackOverflow"      "https://stackoverflow.com/search?q=%s")
                      ("Youtube"            "https://youtube.com/results?aq=f&oq=&search_query=%s")
                      ("Learn X in Y"       "https://learnxinyminutes.com/docs/%s")
                      ("OverAPI"            "https://overapi.com/%s")
                      ("Wolfram alpha"      "https://wolframalpha.com/input/?i=%s")
                      ("Doom Emacs issues"  "https://github.com/hlissner/doom-emacs/issues?q=is%%3Aissue+%s")
                      ("Google images"      "https://www.google.com/images?q=%s")
                      ("Palettes"           "https://www.palettelist.com/")
                      ("Raw"                "%s")
                      ("brew cask"                "https://formulae.brew.sh/cask/%s")
                      ("brew formula"  "https://formulae.brew.sh/formula/%s")
                     )
                    )
(spec-handling-add! lookup-url
                    '(plus
                     ("Google"            +lookup--online-backend-google "https://google.com/search?q=%s")
                     ("Google images"     "https://www.google.com/images?q=%s")
                     ("Google maps"       "https://maps.google.com/maps?q=%s")
                     ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
                     ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
                     ("Wiki Quote"        "https://en.m.wikiquote.org/w/index.php?search=%s")
                     )
                    )
(spec-handling-add! browse-handler
                    '(default
                      ("^@" . browse-select-twitter)
                      ("."  . browse-select-default)
                      )
                    )
(spec-handling-add! lookup-regular
                    '(shell-mode
                     ("Brew"             . "https://brew.sh/")
                     ("Awk"              . "https://www.gnu.org/software/gawk/manual/gawk.html")
                     ("Bash reference"   . "https://www.gnu.org/software/bash/manual/bash.html")
                     ("Sed Reference"    . "https://www.gnu.org/software/sed/manual/sed.html")
                     ("Ripgrep manual"   . "https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md")
                     ("Nushell cmds"     . "https://www.nushell.sh/commands/")
                     ("Nushell"          . "https://www.nushell.sh/book/")
                     ("Nushell cookbook" . "https://www.nushell.sh/cookbook/")
                     ("Nushell github"   . "https://github.com/nushell/nushell")
                     )
                    )
(spec-handling-add! popup
                    '(lookup
                      ("^\\*xref\\*$" :ignore t)
                      )
                    )
(spec-handling-add! lookup-handler
                    `(text-mode
                     :definition ,#'wordnut-search
                     :implementations ,#'helm-wordnet-suggest
                     )
                    )
;;-- end specs
