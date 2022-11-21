;;; +vars.el -*- lexical-binding: t; -*-

(setq-default jg-browse-selected-prog "firefox"
              jg-browse-variant-progs (with-temp-buffer
                                        (insert-file (expand-file-name "~/.browsers"))
                                        (split-string (buffer-string) "\n" t " +")
                                        )

              jg-browse-pdf-args  '("-a" "Preview" "-nF")
              jg-browse-epub-args '("-a" "ebook-viewer")
              jg-browse-curl-cmd  "curl"
              jg-browse-curl-args "-sLI"

              jg-browse-use-preview t
              )

;;-- lookup
(setq jg-browse-providers-alist '(("DuckDuckGo" +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")

                                   ("Scholar" "https://scholar.google.com/scholar?hl=en&q=%s")
                                   ("DBLP"    "https://dblp1.uni-trier.de/search?q=%s")
                                   ("Doi"     "https://doi.org/%s")
                                   ("Github" "https://github.com/search?ref=simplesearch&q=%s")
                                   ("Python" "https://docs.python.org/3/search.html?q=%s&check_keywords=yes&area=default")

                                   ("MDN" "https://developer.mozilla.org/en-US/search?q=%s")
                                   ("Rust Docs" "https://doc.rust-lang.org/std/?search=%s")
                                   ("DevDocs.io" "https://devdocs.io/#q=%s")
                                   ("StackOverflow" "https://stackoverflow.com/search?q=%s")

                                   ("Twitter" "https://twitter.com/%s")
                                   ("Wikipedia" "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
                                   ("Archive.org" "https://archive.org/search.php?query=%s")
                                   ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
                                   ("Youtube" "https://youtube.com/results?aq=f&oq=&search_query=%s")
                                   ("Amazon UK" "https://www.amazon.co.uk/s?k=%s")
                                   ("Amazon US" "https://www.amazon.com/s?k=%s")

                                   ("Learn X in Y" "https://learnxinyminutes.com/docs/%s")
                                   ("OverAPI" "https://overapi.com/%s")
                                   ("Wolfram alpha" "https://wolframalpha.com/input/?i=%s")
                                   ("Doom Emacs issues" "https://github.com/hlissner/doom-emacs/issues?q=is%%3Aissue+%s")
                                   ("Google images" "https://www.google.com/images?q=%s")
                                   ("Palettes" "https://www.palettelist.com/")
                                   ("Raw" "%s")
                                   )
      )
;;-- end lookup

;;-- browsing defaults
(setq-default jg-browse-google-url       "https://duckduckgo.com/?q=%s"
              jg-browse-twitter-url      "https://twitter.com"
              jg-browse-github-url       "https://git-scm.com/doc"
)

(setq-default browse-url-browser-function '+jg-browse-default
              browse-url-default-handlers nil
              ;; alist of (regexp . handler):
              browse-url-handlers
              '(
                ("^@" . +jg-browse-twitter)
                ;; amazon

                ;; default
                ("."  . +jg-browse-default)
                )
              )

;;-- end browsing defaults
