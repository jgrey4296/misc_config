;;; jg-tag/+bindings.el --- summary -*- lexical-binding: t -*-
;;
;; Helm bindings
(defun +jg-tag-binding-hook ()
  (map! :leader
      :prefix ("a h" . "Helms")
      :desc "Firefox Helm"              "f" #'+jg-tag-helm-bookmarks
      :desc "Tag Helm"                  "t" #'+jg-tag-helm-twitter
      :desc "Twitter Helm"              "h" #'+jg-tag-helm-heading-twitter
      :desc "Unified Helm"              "u" #'+jg-tag-helm-unified
      )
  (map! :map helm-map
      "M-SPC" #'helm-next-page)

  (defhydra tag-clean ()
    "
               | Commands   ^^|
               |------------^^|------------^^|
               | [_q_] Quit   | [_!_] Split  |
               | [_f_] Filter | [_p_] Prev   |
               | [_s_] Sub    | [_l_] Leave  |
               "
    ("q" nil :exit t)
    ("f" #'tag-clean/mark-to-filter)
    ("s" #'tag-clean/mark-to-sub)
    ("p" #'tag-clean/previous)
    ("l" #'tag-clean/leave)
    ("!" #'+jg-tag-org-split-on-headings :exit t)
    )
  (map! :map tag-clean-minor-mode-map
        "." #'tag-clean/body
        )
)

(defun +jg-tag-dired-binding-hook ()
  ;; Dired bindings
  (map! :map (dired-mode-map ranger-mode-map)
      :localleader
      (:prefix ("d" . "Describe")
       :desc "Count Untagged Orgs" "u"   #'+jg-tag-dired-directory-count-untagged
       :desc "Describe Marked Tags" "t"  #'+jg-tag-describe-marked-tags
       )
      (:prefix ("m" . "Mark")
       :desc "Mark Untagged Orgs" "u"    #'+jg-tag-mark-untagged-orgs
       )
      (:prefix ("f" . "Find")
       :desc "Display Tag Selection" "s" #'+jg-tag-display-selection
       )
      (:prefix ("i" . "Index")
       :desc "Index People" "p"          #'+jg-tag-index-people
       :desc "Index Tags" "t"            #'+jg-tag-index-tags
       )
  )
)
;; Evil ex commands
(defun +jg-tag-evil-binding-hook ()
  (evil-ex-define-cmd "t[ag]"  #'+jg-tag-helm-start)
  (evil-ex-define-cmd "to"     #'+jg-tag-occurrences)
  (evil-ex-define-cmd "toa"    #'+jg-tag-occurrences-in-open-buffers)
  (evil-ex-define-cmd "tv"     #'org-tags-view)
  (evil-ex-define-cmd "ts"     #'org-set-tags)
  )
