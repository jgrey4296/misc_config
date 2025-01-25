;;; jg-tag/+bindings.el --- summary -*- lexical-binding: t -*-
;;
(map! :map jg-binding-helm-map
      :desc "Twitter Tag Helm"          "t" #'+jg-tag-helm-tag-twitter
      :desc "Twitter Account Helm"      "T" #'+jg-tag-helm-account-twitter
      :desc "Unified Helm"              "u" #'+jg-tag-helm-unified
      :desc "Twitter Grep Helm"         "g" #'+jg-tag-helm-twitter-grep
      )

(map! :map dired-mode-map
      :after jg-dired-bindings
       :desc "Mark Untagged Orgs" "Mu"    #'+jg-tag-dired-mark-untagged-orgs
       :desc "Count Untagged Orgs" "du"   #'+jg-tag-dired-directory-count-untagged
       :desc "Describe Marked Tags" "dt"  #'+jg-tag-dired-describe-marked-tags

      (:prefix ("c f t" . "Tag Index")
       :desc "Index People" "p"          #'+jg-tag-index-people
       :desc "Index Tags"   "t"          #'+jg-tag-index-tags
       )
      )

(map! :leader
      :desc "Open Random Untagged Twitter" "o u" #'+jg-tag-open-random-untagged-twitter
      )


(after! jg-evil-ex-bindings
  (evil-ex-define-cmd "ht[ag]"  #'librarian-tag-helm)
  (evil-ex-define-cmd "t[ag]"   #'librarian-tag-helm)
  (evil-ex-define-cmd "T[ag]"   #'librarian-tag-helm)
  (evil-ex-define-cmd "it[ag]"  #'librarian-tag-ivy)
  )
