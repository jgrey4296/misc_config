;;; +vars.el -*- lexical-binding: t; -*-

(after! librarian
  (add-to-list 'librarian-active-on-modes 'conf-mode)
  (add-to-list 'librarian-active-on-modes 'comint-mode)
  )

(speckler-setq! librarian ()
  librarian-regular-loc                        (expand-file-name "librarian-regular" templates-loc)
  librarian--config-modules-dir                (expand-file-name "modules" doom-user-dir)
  librarian--man-path                          (expand-file-name "man/main" templates-loc)
  librarian--man-config                        (expand-file-name "tools/man.conf" templates-loc)
  librarian--man-cache-dir                     (expand-file-name ".temp" doom-user-dir)

  librarian-tag-mode-substitution-sources      (expand-file-name "~/github/bibliography/tags/substitutions")
  librarian--tag-mode-main-loc                 (expand-file-name "~/github/bibliography/.temp/tags/canon.tags")
  )

;;-- specs
(speckler-add! lookup-url ()
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
    ("Palettes"                                                "https://www.palettelist.com/")
    ("Raw"                                                     "%s")
    ("brew cask"                                               "https://formulae.brew.sh/cask/%s")
    ("brew formula"                                            "https://formulae.brew.sh/formula/%s")
    ("Maps"                                                    "https://www.davidrumsey.com/luna/servlet/view/search?annotSearch=annotSearch&q=%s")
    ("Localhost"                                               "127.0.0.1:8000")
    ("Router"                                                  "192.168.1.1")
    )
  )
(speckler-add! lookup-url ()
  '(plus
    ("Google"            librarian-backend--online-google "https://google.com/search?q=%s")
    ("Google images"     "https://www.google.com/images?q=%s")
    ("Google maps"       "https://maps.google.com/maps?q=%s")
    ("Google Ngrams"     "https://books.google.com/ngrams/graph?content=%s&year_start=1800&year_end=2019&corpus=en-2019&smoothing=3")
    ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
    ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
    ("Wiki Quote"        "https://en.m.wikiquote.org/w/index.php?search=%s")
    ("Down For Everyone?" "https://downforeveryoneorjustme.com/%s")
    )
  )
(speckler-add! browse-handler ()
  '(default
    ("."  . librarian-browse-open)
    )
  )
(speckler-add! popup ()
  '(lookup
    ("^\\*xref\\*$" :ignore t)
    )
  )
(speckler-add! lookup-handler ()
  `(text-mode
    :definition #'wordnut-search
    :implementations #'helm-wordnet-suggest
    )
  )
;;-- end specs
