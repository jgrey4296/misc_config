;;; jg-tag/+bindings.el --- summary -*- lexical-binding: t -*-
;;
;;-- helm
(map! :map jg-binding-helm-map
      :desc "Firefox Helm"              "f" #'+jg-tag-helm-bookmarks
      :desc "Twitter Tag Helm"          "t" #'+jg-tag-helm-tag-twitter
      :desc "Twitter Account Helm"      "T" #'+jg-tag-helm-account-twitter
      :desc "Unified Helm"              "u" #'+jg-tag-helm-unified
      :desc "Twitter Grep Helm"         "g" #'+jg-tag-helm-twitter-grep
      )

(map! :after helm
      :map helm-map
      "M-SPC" #'helm-next-page
      :localleader
      :desc "Save Results" "s" #'+jg-tag-save-helm-buffer
      )

;;-- end helm

;; Dired bindings
(map! :after 'jg-dired-bindings
      :map dired-mode-map
      :localleader
      (:prefix ("d" . "Describe")
       :desc "Count Untagged Orgs" "u"   #'+jg-tag-dired-directory-count-untagged
       :desc "Describe Marked Tags" "t"  #'+jg-tag-describe-marked-tags
       )
      (:prefix ("m" . "Mark")
       :desc "Mark Untagged Orgs" "u"    #'+jg-tag-mark-untagged-orgs
       )
      (:prefix ("f" . "Find"))
      (:prefix ("i" . "Index")
       :desc "Index People" "p"          #'+jg-tag-index-people
       :desc "Index Tags" "t"            #'+jg-tag-index-tags
       )
      )
;; Evil ex commands
(evil-ex-define-cmd "t[ag]"  #'+jg-tag-helm-start)
(evil-ex-define-cmd "to"     #'+jg-tag-occurrences)
(evil-ex-define-cmd "toa"    #'+jg-tag-occurrences-in-open-buffers)
(evil-ex-define-cmd "tv"     #'org-tags-view)
(evil-ex-define-cmd "ts"     #'org-set-tags)

(map! :leader
      :desc "Open Random Untagged Twitter" "o u" #'+jg-tag-open-random-untagged-twitter
      )
