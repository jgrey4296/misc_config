;;; editor/project-walk/+bindings.el -*- lexical-binding: t; -*-

(map! :after jg-leader-bindings-loaded
      :leader
      :prefix "p"
      :desc "Project Walk" "W" #'project-walk-minor-mode
      :desc "Walk to next" "n" #'project-walk-next
      (:prefix ("w" . "Project Walk..")
       :desc "Project Remaining Num"   "r"   #'project-walk-num
       :desc "Project Remaining"       "R"   #'project-walk-remaining
       :desc "Directory Walk"          "d"   #'project-walk-directory-init
       :desc "Filter Defaults"         "SPC" #'project-walk-filter-defaults
       :desc "Filter"                  "f"   #'project-walk-filter
       :desc "Keep"                    "k"   #'project-walk-filter-keep
       )
      )
