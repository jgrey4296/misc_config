;;; editor/project-walk/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :prefix "p"
       :desc "Project Walk" "W" #'project-walk-minor-mode
       :desc "Walk to next" "n" #'project-walk-next
       (:prefix ("w" . "Project Walk..")
        :desc "Project Remaining Num" "r" #'project-walk-num
        :desc "Project Remaining"     "R" #'project-walk-remaining
        :desc "Project Filter"        "f" #'project-walk-filter
        :desc "Directory Walk"        "d" #'project-walk-directory-init
        )
      )

(map! :leader
      :prefix "s"
      :desc "Replace in Project"        "r" #'projectile-replace
      :desc "Replace Regexp in Project" "R" #'projectile-replace-regexp
      )
