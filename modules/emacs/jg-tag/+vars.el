;;; main/jg-tag/+vars.el -*- lexical-binding: t; -*-

(setq-default jg-tag-candidate-counts '()
              jg-tag-candidates-names '()
              jg-tag-global-tags (make-hash-table :test 'equal)
              jg-tag-alt-mapping (make-hash-table)

              jg-tag-loc-bookmarks             "~/github/writing/resources/bookmarks/main_bookmarks.html"
              jg-tag-loc-global-tags           "~/github/writing/resources/cron_reports/totals.tags"
              jg-tag-loc-twitter-account-index "~/github/writing/resources/cron_reports/tw_acct.index"
              jg-tag-loc-twitter-tag-index     "~/github/writing/resources/cron_reports/tw_tag.index"
              jg-tag-loc-default-helm-directory "~/github/writing/resources/"

              jg-tag-marker (make-marker)
              jg-tag-twitter-heading-helm-candidates nil
              jg-tag-twitter-helm-candidates nil
              jg-tag-helm-buffer-name "*Helm Tags*"
              )
