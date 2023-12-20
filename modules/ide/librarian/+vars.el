;;; +vars.el -*- lexical-binding: t; -*-


(setq librarian-regular--location                  (expand-file-name "templates/lookup-regular" doom-user-dir)
      librarian-configs--modules-dir               (expand-file-name "modules" doom-user-dir)
      librarian-man--path                          (expand-file-name "templates/man/main" doom-user-dir)
      librarian-man--config                        (expand-file-name "templates/tools/man.conf" doom-user-dir)
      librarian-man--cache-dir                     (expand-file-name ".temp" doom-user-dir)

      librarian-tagging-mode-substitution-sources  (expand-file-name "~/github/bibliography/tags/substitutions")
      librarian-tagging-mode-main-loc              (expand-file-name "~/github/bibliography/.temp/tags/totals.tags")
      )


;;-- specs
(spec-handling-add! lookup-url
                    '(defaults
                      ("DuckDuckGo"         librarian-backend--online-duckduckgo "https://duckduckgo.com/?q=%s")
                      ("Github"                                                  "https://github.com/search?ref=simplesearch&q=%s")
                      ("DevDocs.io"                                              "https://devdocs.io/#q=%s")
                      ("StackOverflow"                                           "https://stackoverflow.com/search?q=%s")
                      ("Youtube"                                                 "https://youtube.com/results?aq=f&oq=&search_query=%s")
                      ("Learn X in Y"                                            "https://learnxinyminutes.com/docs/%s")
                      ("OverAPI"                                                 "https://overapi.com/%s")
                      ("Wolfram alpha"                                           "https://wolframalpha.com/input/?i=%s")
                      ("Doom Emacs issues"                                       "https://github.com/hlissner/doom-emacs/issues?q=is%%3Aissue+%s")
                      ("Google images"                                           "https://www.google.com/images?q=%s")
                      ("Palettes"                                                "https://www.palettelist.com/")
                      ("Raw"                                                     "%s")
                      ("brew cask"                                               "https://formulae.brew.sh/cask/%s")
                      ("brew formula"                                            "https://formulae.brew.sh/formula/%s")
                      ("Ngrams"                                                  "https://books.google.com/ngrams/graph?content=%s&year_start=1800&year_end=2019&corpus=en-2019&smoothing=3")
                      ("Localhost"                                               "127.0.0.1:8000")
                      ("Router"                                                  "192.168.1.1")
                     )
                    )
(spec-handling-add! lookup-url
                    '(plus
                     ("Google"            libraria-backend--online-google "https://google.com/search?q=%s")
                     ("Google images"     "https://www.google.com/images?q=%s")
                     ("Google maps"       "https://maps.google.com/maps?q=%s")
                     ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
                     ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
                     ("Wiki Quote"        "https://en.m.wikiquote.org/w/index.php?search=%s")
                     )
                    )
(spec-handling-add! browse-handler
                    '(default
                      ("."  . librarian-browser--open-url)
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
