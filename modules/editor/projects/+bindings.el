;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :prefix ("p" . "project")
      :desc "Run cmd in project root"      "!"  #'projectile-run-shell-command-in-root
      :desc "Project Root"                 "`"  (cmd! (find-file (doom-project-root)))
      :desc "Root Shell"                   "'"  #'projectile-run-shell
      :desc "Search project for symbol"    "."  #'+default/search-project-for-symbol-at-point
      :desc "Walk to next"                 "n"  #'project-walk-next

      :desc "Find file in project sidebar" "s"    #'+neotree/find-this-file
      :desc "Compile in project"           "c"  #'projectile-compile-project
      :desc "List project todos"           "t"  #'magit-todos-list
      :desc "Open project scratch buffer"  "x"  #'+jg-ui-open-scratch-buffer

      :desc "Find file in project"         "f"  #'projectile-find-file
      :desc "Find other file"              "o"  #'+jg-projects-find-related

      :desc "Print Project Type"           "?" #'+jg-projects-detect-type

      ;; later expanded by projectile
      (:prefix ("4" . "in other window"))
      (:prefix ("5" . "in other frame"))
      )

;;-- management
(map! :leader
      :prefix ("p m" . "management")
       :desc "Invalidate project cache"     "C"  #'projectile-invalidate-cache
       :desc "Browse other project"         ">"  #'doom/browse-in-other-project
       :desc "Add new project"              "a"  #'projectile-add-known-project
       :desc "Discover projects in folder"  "d"  #'+default/discover-projects
       :desc "Clear Known Projects"         "D"  #'projectile-clear-known-projects
       :desc "Switch project"               "p"  #'projectile-switch-project
      )

;;-- end management

;;-- project wide
(map! :leader
      :prefix ("p ;" . "Project Wide")
       :desc "Replace in Project"        "r" #'projectile-replace
       :desc "Replace Regexp in Project" "R" #'projectile-replace-regexp

       :desc "Save project files"           "S"  #'projectile-save-project-buffers
       :desc "Kill project buffers"         "K"  #'projectile-kill-buffers
       )
;;-- end project wide

;;-- run commands
(map! :leader
      :prefix ("p d" . "Do something")
      :desc "Clean"                        "c"  #'+jg-projects-clean
      :desc "Run project"                  "r"  #'projectile-run-project
      :desc "Test project"                 "t"  #'projectile-test-project
      )
;;-- end run commands

;;-- find files
(map! :leader
      :prefix ("p b" . "browse")
      :desc "Open project editorconfig"    "e"    #'editorconfig-find-current-editorconfig
      :desc "Edit project .dir-locals"     "l"    #'projectile-edit-dir-locals
      :desc "Configure project"            "c"    #'+jg-projects-open-configs
      :desc "Find recent project files"    "r"    #'projectile-recentf

      (:when (modulep! :os macos)
       :desc "Reveal project in Finder"   "f"     #'+macos/reveal-project-in-finder
       )
      )
;;-- end find files

;;-- project walk
(map! :leader
      :prefix ("p b w" . "Project Walk..")
       :desc "Activate Project Walk"            "RET" #'project-walk-minor-mode
       :desc "Filter Defaults"                  "SPC" #'project-walk-filter-defaults
       :desc "Remaining Num"                    "r"   #'project-walk-num
       :desc "Remaining"                        "R"   #'project-walk-remaining
       :desc "Directory Walk"                   "d"   #'project-walk-directory-init
       :desc "Filter"                           "f"   #'project-walk-filter
       :desc "Keep"                             "k"   #'project-walk-filter-keep
      )

;;-- end project walk
