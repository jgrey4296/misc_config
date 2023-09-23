;;; jg-tag/+bindings.el --- summary -*- lexical-binding: t -*-
;;
(map! :map jg-binding-helm-map
      :desc "Twitter Tag Helm"          "t" #'+jg-tag-helm-tag-twitter
      :desc "Twitter Account Helm"      "T" #'+jg-tag-helm-account-twitter
      :desc "Unified Helm"              "u" #'+jg-tag-helm-unified
      :desc "Twitter Grep Helm"         "g" #'+jg-tag-helm-twitter-grep
      )

(map! :map helm-map
      :after helm
      "M-SPC" #'helm-next-page
      :localleader
      :desc "Save Results" "s" #'+jg-tag-save-helm-buffer
      )

;; Dired bindings
(map! :map dired-mode-map
      :after jg-dired-bindings
       :desc "Mark Untagged Orgs" "Mu"    #'+jg-tag-dired-mark-untagged-orgs
       :desc "Count Untagged Orgs" "du"   #'+jg-tag-dired-directory-count-untagged
       :desc "Describe Marked Tags" "dt"  #'+jg-tag-dired-describe-marked-tags

      (:prefix (">i" . "Index")
       :desc "Index People" "p"          #'+jg-tag-index-people
       :desc "Index Tags" "t"            #'+jg-tag-index-tags
       )
      )

(after! jg-evil-ex-bindings
  (evil-ex-define-cmd "t[ag]"  #'librarian-tagging-mode-tagger)
  ;; (evil-ex-define-cmd "tv"     #'org-tags-view)
  ;; (evil-ex-define-cmd "ts"     #'org-set-tags)
  )

(map! :leader
      :desc "Open Random Untagged Twitter" "o u" #'+jg-tag-open-random-untagged-twitter
      )
