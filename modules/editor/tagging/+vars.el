;;; main/jg-tag/+vars.el -*- lexical-binding: t; -*-


(defvar jg-tag-loc-default-helm-directory (expand-file-name "~/github/bibliography/"))
(defvar jg-tag-loc-global-tags            (expand-file-name "~/github/bibliography/tags/substitutions"))
(defvar jg-tag-loc-twitter-account-index  (expand-file-name "~/github/bibliography/.temp/index/tw_acct.index"))
(defvar jg-tag-loc-twitter-grep-index     (expand-file-name "~/github/bibliography/.temp/index/grep_tags.index"))
(defvar jg-tag-loc-twitter-tag-index      (expand-file-name "~/github/bibliography/.temp/index/tw_tag.index"))
(defvar jg-tag-loc-twitter                "/Volumes/documents/twitter_threads/")


;;-- popup

(spec-handling-add! popup
                    '(tagging
                      ("^\\*Helm-Bookmark-Results\\*"  :side right :ttl nil :width 0.4 :quit t :select nil :priority 50)
                     )
                    )
;;-- end popup
