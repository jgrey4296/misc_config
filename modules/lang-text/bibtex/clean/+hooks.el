;;; +hooks.el -*- lexical-binding: t; -*-

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
