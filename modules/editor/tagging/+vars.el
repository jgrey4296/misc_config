;;; main/jg-tag/+vars.el -*- lexical-binding: t; -*-


(defvar jg-tag-loc-bookmarks              (expand-file-name "~/github/jgrey4296.github.io/resources/bookmarks/total.bookmarks"))
(defvar jg-tag-loc-default-helm-directory (expand-file-name "~/github/jgrey4296.github.io/resources/"))
(defvar jg-tag-loc-global-tags            (expand-file-name "~/github/jgrey4296.github.io/resources/tags/substitutions"))
(defvar jg-tag-loc-twitter-account-index  (expand-file-name "~/github/jgrey4296.github.io/.temp/index/tw_acct.index"))
(defvar jg-tag-loc-twitter-grep-index     (expand-file-name "~/github/jgrey4296.github.io/resources/index/grep_tags.index"))
(defvar jg-tag-loc-twitter-tag-index      (expand-file-name "~/github/jgrey4296.github.io/resources/index/tw_tag.index"))
(defvar jg-tag-loc-twitter                "/Volumes/documents/twitterthreads/")

(defvar jg-tag-twitter-heading-helm-candidates nil)
(defvar jg-tag-twitter-helm-candidates nil)
(defvar jg-tag-twitter-grep-helm-candidates nil)


(defvar jg-tag-bookmark-helm-source)
(defvar jg-tag-twitter-helm-source)
(defvar jg-tag-file-select-source)
(defvar jg-tag-twitter-grep-helm-source)

;;-- popup
(after! jg-ui-reapply-hook-ready
  (+jg-popup-add-spec 'tagging
                         '(("^\\*Helm-Bookmark-Results\\*"  :side right :ttl nil :width 0.4 :quit t :select nil :priority 50)
                           )
                         )
  )
;;-- end popup
