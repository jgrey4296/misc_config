;;; main/jg-tag/+vars.el -*- lexical-binding: t; -*-

(setq-default jg-tag-candidate-counts '()
              jg-tag-candidates-names '()
              jg-tag-global-tags (make-hash-table :test 'equal)
              jg-tag-alt-mapping (make-hash-table)

              jg-tag-loc-bookmarks             "~/github/writing/resources/main_bookmarks.html"
              jg-tag-loc-global-tags           "~/github/writing/resources/collate.tags"
              jg-tag-loc-twitter-account-index "~/.doom.d/setup_files/tw_acct.index"
              jg-tag-loc-twitter-tag-index     "~/.doom.d/setup_files/tw_tag.index"
              jg-tag-loc-default-helm-directory "~/github/writing/resources/"

              jg-tag-marker (make-marker)
              jg-tag-twitter-heading-helm-candidates nil
              jg-tag-twitter-helm-candidates nil
              )
