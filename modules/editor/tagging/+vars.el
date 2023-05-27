;;; main/jg-tag/+vars.el -*- lexical-binding: t; -*-


(defvar jg-tag-loc-default-helm-directory (expand-file-name "~/github/jgrey4296.github.io/resources/"))
(defvar jg-tag-loc-global-tags            (expand-file-name "~/github/jgrey4296.github.io/resources/tags/substitutions"))
(defvar jg-tag-loc-twitter-account-index  (expand-file-name "~/github/jgrey4296.github.io/.temp/index/tw_acct.index"))
(defvar jg-tag-loc-twitter-grep-index     (expand-file-name "~/github/jgrey4296.github.io/resources/index/grep_tags.index"))
(defvar jg-tag-loc-twitter-tag-index      (expand-file-name "~/github/jgrey4296.github.io/resources/index/tw_tag.index"))
(defvar jg-tag-loc-twitter                "/Volumes/documents/twitter_threads/")

(defvar jg-tag-twitter-heading-helm-candidates nil)
(defvar jg-tag-twitter-helm-candidates nil)
(defvar jg-tag-twitter-grep-helm-candidates nil)


(defvar jg-tag-twitter-helm-source)
(defvar jg-tag-file-select-source)
(defvar jg-tag-twitter-grep-helm-source)

;;-- popup

(spec-handling-add! popup
                    '(tagging
                      ("^\\*Helm-Bookmark-Results\\*"  :side right :ttl nil :width 0.4 :quit t :select nil :priority 50)
                     )
                    )
;;-- end popup
