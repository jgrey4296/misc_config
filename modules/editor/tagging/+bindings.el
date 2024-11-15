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

;;-- gtags
(map! :map jg-binding-jump-map
      :prefix ("g" . "gtags")
      :desc "Create Tags"           "c" #'helm-gtags-create-tags
      :desc "Find Symbol"           "y" #'helm-gtags-find-symbol
      :desc "Find Tag Other Window" "o" #'helm-gtags-find-tag-other-window
      :desc "Find Tag"              "d" #'helm-gtags-find-tag
      :desc "Find rtag"             "r" #'helm-gtags-find-rtag
      :desc "Gtags Select"          "s" #'helm-gtags-select
      :desc "Parse File"            "p" #'helm-gtags-parse-file
      :desc "Tags in func"          "i" #'helm-gtags-tags-in-this-function
      :desc "Update Tags"           "u" #'helm-gtags-update-tags
      )

;;-- end gtags

(after! jg-evil-ex-bindings
  (evil-ex-define-cmd "ht[ag]"  #'librarian-tagging-helm)
  (evil-ex-define-cmd "t[ag]"  #'librarian-tagging-helm)
  (evil-ex-define-cmd "T[ag]"  #'librarian-tagging-helm)
  (evil-ex-define-cmd "it[ag]"  #'librarian-tagging-ivy)
  )
