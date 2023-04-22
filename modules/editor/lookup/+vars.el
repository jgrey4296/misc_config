;;; +vars.el -*- lexical-binding: t; -*-

;;-- handlers
(defvar +lookup-provider-url-alist nil
  "An alist that maps online resources to either:

  1. A search url (needs on '%s' to substitute with an url encoded query),
  2. A non-interactive function that returns the search url in #1,
  3. An interactive command that does its own search for that provider.

Used by `+lookup/online'.")

(defvar +lookup-open-url-fn #'browse-url
  "Function to use to open search urls.")

(defvar +lookup-definition-functions
  '(+lookup-dictionary-definition-backend-fn
    +lookup-xref-definitions-backend-fn
    +lookup-dumb-jump-backend-fn
    +lookup-project-search-backend-fn
    +lookup-evil-goto-definition-backend-fn)
  "Functions for `+lookup/definition' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-implementations-functions ()
  "Function for `+lookup/implementations' to try. Stops at the first function to
return non-nil or change the current window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-type-definition-functions ()
  "Functions for `+lookup/type-definition' to try. Stops at the first function to
return non-nil or change the current window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-references-functions
  '(+lookup-thesaurus-definition-backend-fn
    +lookup-xref-references-backend-fn
    +lookup-project-search-backend-fn)
  "Functions for `+lookup/references' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-documentation-functions
  '(+lookup-online-backend-fn)
  "Functions for `+lookup/documentation' to try, before resorting to
`dumb-jump'. Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-file-functions
  '(+lookup-bug-reference-backend-fn
    +lookup-ffap-backend-fn)
  "Function for `+lookup/file' to try, before restoring to `find-file-at-point'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-dictionary-prefer-offline (modulep! +offline)
  "If non-nil, look up dictionaries online.

Setting this to nil will force it to use offline backends, which may be less
than perfect, but available without an internet connection.

Used by `+lookup/dictionary-definition' and `+lookup/synonyms'.

For `+lookup/dictionary-definition', this is ignored on Mac, where Emacs users
Dictionary.app behind the scenes to get definitions.")

;;-- end handlers

(setq jg-browse-selected-prog "firefox"
      jg-browse-variant-progs (with-temp-buffer
                                (insert-file (expand-file-name "~/.browsers"))
                                (split-string (buffer-string) "\n" t " +")
                                )

      jg-browse-pdf-args  '("-a" "Preview" "-nF")
      jg-browse-epub-args '("-a" "ebook-viewer")
      jg-browse-curl-cmd  "curl"
      jg-browse-curl-args "-sLI"

      jg-browse-use-preview t

      browse-url-browser-function '+jg-browse-default
      browse-url-default-handlers nil
      )

;;-- specs
(spec-handling-add! lookup-url nil
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
                       )
                     )
(spec-handling-add! lookup-url nil
                    '(plus
                     ("Google"            +lookup--online-backend-google "https://google.com/search?q=%s")
                     ("Google images"     "https://www.google.com/images?q=%s")
                     ("Google maps"       "https://maps.google.com/maps?q=%s")
                     ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
                     ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
                     ("Rust Docs" "https://doc.rust-lang.org/std/?search=%s")
                     )
                    )
(spec-handling-add! browse-handler nil
                    '(default
                      ("^@" . +jg-browse-twitter)
                      ("."  . +jg-browse-default)
                      )
                    )
(spec-handling-add! lookup-regular nil
                    '(shell-mode
                     ("Brew" . "https://brew.sh/")
                     ("Awk" . "https://www.gnu.org/software/gawk/manual/gawk.html")
                     ("Bash reference" . "https://www.gnu.org/software/bash/manual/bash.html")
                     ("Sed Reference" . "https://www.gnu.org/software/sed/manual/sed.html")
                     ("Ripgrep manual" . "https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md")
                     ("Nushell cmds" . "https://www.nushell.sh/commands/")
                     ("Nushell" . "https://www.nushell.sh/book/")
                     ("Nushell cookbook" . "https://www.nushell.sh/cookbook/")
                     ("Nushell github" . "https://github.com/nushell/nushell")
                     )
                    )
;;-- end specs
