;;; editor/project-walk/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :prefix "p"
      :desc "Project Walk" "W" #'project-walk-minor-mode
      :desc "Walk to next" "n" #'project-walk-next
      (:prefix ("w" . "Project Walk..")
       :desc "Project Remaining Num" "r" #'project-walk-num
       :desc "Project Remaining"     "R" #'project-walk-remaining
       :desc "Directory Walk"        "d" #'project-walk-directory-init
       )
       (:prefix ("wf" . "Filter")
        :desc "Filter Defaults" "SPC" #'project-walk-filter-defaults
        :desc "Filter Name"     "n" #'project-walk-filter-name
        :desc "Filter Ext"      "e" #'project-walk-filter-ext
        :desc "Filter Dir"      "d" #'project-walk-filter-dir
        :desc "Keep"            "k" #'project-walk-filter-keep
        :desc "Filter Regexp"   "r" #'project-walk-filter-regexp
        :desc "Keep Regexp"     "R" #'project-walk-filter-keep-regexp
        )
       )

(map! :leader
      :prefix "s"
      :desc "Replace in Project"        "r" #'projectile-replace
      :desc "Replace Regexp in Project" "R" #'projectile-replace-regexp
      )
