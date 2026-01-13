;;; +bindings.el -*- lexical-binding: t; -*-

;; Delete the current workspace if closing the last open window

(define-key! persp-mode-map
  [remap delete-window] #'+workspace/close-window-or-workspace
  [remap evil-window-delete] #'+workspace/close-window-or-workspace)

(map! :leader
      :desc "Carousel Control"         "R"      #'jg-workspace-run-carousel-transient
      :desc "Workspace Counsel"        "W"      #'+jg-workspaces-ivy
      (:prefix ("w" . "Windows")
       :desc "Workspace Control"        "RET"   #'+jg-workspace-run-transient
       :desc "Delete workspace"         "DEL"   #'+workspace/delete
       :desc "Workspace Control"        "w"     #'+jg-workspace-run-transient
       :desc "root shell"               "'"     #'projectile-run-shell
       )
      (:prefix ("h" . "Help")
       :desc "Reload Persp" "r P" #'+jg-workspaces-rebuild-persp-cache
       )
      )

(map! :map jgb-jump--root-map
      :desc "Jump to related"              "r"   #'+jg-projects-find-related
      :desc "Goto Root"                    "/ `" (cmd! (find-file (projectile-project-root)))
      :desc "Search project"               "/ p" #'+jg-workspaces-search-project
      :desc "Search project for symbol"    "/ ." #'+jg-workspaces-search-project-for-symbol-at-point
      :desc "Find File in Project"         "/ f" #'projectile-find-file
      :desc "Find test file"               "/ t" #'+jg-projects-test-files
      :desc "Find Related"                 "/ r" #'+jg-projects-find-related
      )

(map! :map jgb-help-map
      :after jg-help-bindings
      "d p" #'+jg-projects-detect-type
  )

(map! :map jg-dired-mode-map
      :localleader
      :desc "Project File" "g p" (cmd! (+jg-dired-touch ".project"))
      )

(local-load! "+bind-extra")
