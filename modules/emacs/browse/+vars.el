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
              jg-browse-google-url       "https://duckduckgo.com/?q=%s"
              jg-browse-twitter-url      "https://twitter.com"
              jg-browse-github-url       "https://git-scm.com/doc"

              browse-url-browser-function '+jg-browse-default
              browse-url-default-handlers nil
              )

;;-- lookup
(+jg-browse-add-lookup-spec 'default
                     '(
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
                       )
                     )
;;-- end lookup

;;-- browsing defaults
(+jg-browse-add-handler-spec
 'default
 '(
   ("^@" . +jg-browse-twitter)
   ;; todo  amazon

   ;; default
   ("."  . +jg-browse-default)
   )
 )
;;-- end browsing defaults
