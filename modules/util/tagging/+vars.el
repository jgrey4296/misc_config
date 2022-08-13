;;; main/jg-tag/+vars.el -*- lexical-binding: t; -*-

(setq-default jg-tag-candidate-counts '()
              jg-tag-candidates-names '()
              jg-tag-global-tags (make-hash-table :test 'equal)
              jg-tag-alt-mapping (make-hash-table)

              jg-tag-loc-bookmarks              "~/github/writing/resources/bookmarks/total.bookmarks"
              jg-tag-loc-global-tags            "~/github/writing/resources/cron_reports/totals_total.tags"
              jg-tag-loc-twitter-account-index  "~/github/writing/resources/cron_reports/tw_acct.index"
              jg-tag-loc-twitter-tag-index      "~/github/writing/resources/cron_reports/tw_tag.index"
              jg-tag-loc-default-helm-directory "~/github/writing/resources/"
              jg-tag-loc-twitter                "/Volumes/documents/twitterthreads/"
              jg-tag-loc-twitter-grep-index     "~/github/writing/resources/cron_reports/grep_tags.index"

              jg-tag-marker (make-marker)
              jg-tag-twitter-heading-helm-candidates nil
              jg-tag-twitter-helm-candidates nil
              jg-tag-twitter-grep-helm-candidates nil
              jg-tag-helm-buffer-name "*Helm Tags*"

              jg-tag-all-loc "~/github/writing/resources/cron_reports/totals_total.tags"
              )

(defvar jg-tag-bookmark-helm-source)
(defvar jg-tag-twitter-helm-source)
(defvar jg-tag-file-select-source)
(defvar jg-tag-helm-source)
(defvar jg-tag-fallback-source)
(defvar jg-tag-twitter-grep-helm-source)

(defvar jg-tag-all-tags nil)

;;-- popup
(setq jg-tag-popup-rules
      '(("^\\*Helm-Bookmark-Results\\*"  :side right :ttl nil :width 0.4 :quit t :select nil :priority 50)
        ))
(after! jg-popup-init
  (+jg-ui-popup-add-rules 'helm-tags jg-tag-popup-rules)
  )
;;-- end popup
