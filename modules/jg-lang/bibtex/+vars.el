;;; domain-specific/bibtex/+vars.el -*- lexical-binding: t; -*-

;;-- personal settings
(setq-default jg-bibtex-indent-equals-column 14
              jg-bibtex-fill-column 50000

              jg-bibtex-helm-candidates nil
              jg-bibtex-candidates-names '()

              jg-bibtex-rand-log ".emacs_rand_bib_log"
              jg-bibtex-tweet-rand-log ".emacs_tweet_rand_bib_log"

              jg-bibtex-clean-move-entry-on-fail nil
              jg-bibtex-open-doi-with-pdf        nil
              jg-bibtex-open-url-with-pdf        nil
              )

;;-- end personal settings

;;-- general bibtex settings
(after! jg-bibtex-vars-go
  (setq-default bibtex-user-optional-fields          nil
                bibtex-completion-bibliography       nil
                bibtex-field-indentation             1
                bibtex-text-indentation              15
                bibtex-align-at-equal-sign           t
                org-bibtex-export-arbitrary-fields   t
                )
  )
;;-- end general bibtex settings

;;-- bibtex fields
(setq jg-bibtex-search-fields               '("tags" "year" "publisher")
      jg-bibtex-scholar-search-fields       '("author" "editor" "ALTauthor" "Alteditor" "year" "doi" "isbn")
      jg-bibtex-scholar-search-fields-exact '("title")
      )
(after! jg-bibtex-vars-go
  (setq bibtex-completion-additional-search-fields jg-bibtex-search-fields
        bibtex-completion-pdf-field                "file")
  )
;;-- end bibtex fields

;;-- cleaning hooks
(setq jg-bibtex-clean-hooks '(+jg-bibtex-insert-stub-key ;; Initial key
                              ;; Initial formatting
                              +jg-bibtex-remove-empty-fields
                              +jg-bibtex-dont-break-lines-hook
                              +jg-bibtex-smart-replace-nonascii-hook
                              +jg-bibtex-orcb-&

                              ;; Specific fields
                              ;; org-ref-title-case-article
                              +jg-bibtex-clean-doi-hook
                              +jg-bibtex-check-file-hook
                              +jg-bibtex--expand-shortened-url
                              +jg-bibtex-isbn-clean
                              ;; generate key
                              +jg-bibtex-orcb-key-hook
                              +jg-bibtex-insert-volume-to-key
                              ;; Final alignment and indent
                              +jg-bibtex-clean-whitespace-hook
                              +jg-bibtex-align-hook
                              +jg-bibtex-indent-hook
                              )
      )
;;-- end cleaning hooks

;;-- urls
(setq jg-bibtex-reference-url         "https://www.bibtex.com/g/bibtex-format/"
      jg-bibtex-doi-url               "https://doi.org/%s"
      )
;;-- end urls

;;-- strings
(setq jg-bibtex-completion-display-formats
      '(
        (t . ("${author:20} || ${title:*} || ${year:4}" 40))
        )
      )
(setq jg-bibtex-pdf-loc-regexp               "file[[:digit:]]*\s*=\s*{\\(.+\\)/\\(.+pdflibrary\\)?"
      jg-bibtex-pdf-replace-match-string     "~/"
      jg-bibtex-pdf-replace-library-string   "pdflibrary"
      jg-bibtex-remove-field-newlines-regexp "^="
      jg-bibtex-tweet-pattern                "(%s): %s\nby %s\nTags: %s\nRef: %s\n"
 )
;;-- end strings

;;-- locations
(setq-default jg-bibtex-loc-bibtex          "~/github/writing/resources/bibliography/"
              jg-bibtex-todo-loc            "~/github/writing/resources/todo.bib"
              jg-bibtex-loc-completions     "~/github/writing/resources/completions/"
              jg-bibtex-loc-export-bib-file "~/github/emacs_files/modules/jg-lang/bibtex/export_template.tex"
              jg-bibtex-loc-temp-dir        "~/.tex/"
              jg-bibtex-pdf-loc             "~/pdflibrary"
 )
;;-- end locations

;;-- commands
(setq jg-bibtex-curl-cmd      "curl"
      jg-bibtex-curl-args     "-sLI"
      ;; bibtex-completion-pdf-open-function #'(lambda (x) (org-open-link-from-string (format "[[file:%s]]" x)))
      bibtex-completion-pdf-open-function 'browse-url
 )
;;-- end commands

;;-- hl line
(after! hl-line
  (add-to-list 'global-hl-line-modes 'bibtex-mode)
  )
;;-- end hl line
