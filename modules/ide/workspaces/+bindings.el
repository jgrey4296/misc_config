;;; +bindings.el -*- lexical-binding: t; -*-

;; Delete the current workspace if closing the last open window

(define-key! persp-mode-map
  [remap delete-window] #'+workspace/close-window-or-workspace
  [remap evil-window-delete] #'+workspace/close-window-or-workspace)

(map! :leader
      :desc "Workspace Control"        "W"     #'+jg-workspace-run-transient
      :desc "Carousel Control"         "R"     #'jg-workspace-run-carousel-transient
      :desc "Workspace Counsel"        "w RET" #'+jg-workspaces-ivy
      (:prefix ("w" . "Windows")
       :desc "Delete workspace"             "DEL"   #'+workspace/delete
       :desc "Workspace Control"            "w"     #'+jg-workspace-run-transient
       )
      (:prefix "p"
       :desc "Goto Root"                    "`"     (cmd! (find-file (projectile-project-root)))
       )
      )

(map! :leader
      :prefix ("p" . "project")
      :desc "run cmd in project root"      "!"    #'projectile-run-shell-command-in-root
      :desc "root shell"                   "'"    #'projectile-run-shell
      :desc "search project for symbol"    "."    #'+jg-workspaces-search-project-for-symbol-at-point
      :desc "find file in project"         "f"    #'projectile-find-file
      :desc "find other file"              "o"    #'+jg-projects-find-related
      :desc "compile in project"           "c"    #'projectile-compile-project
      :desc "Search project"               "p"    #'+default/search-project
      )

(map! :map jg-binding-jump-map
      :desc "Search project"               "/ p" #'+jg-workspaces-search-project
      :desc "Search project for symbol"    "/ ." #'+jg-workspaces-search-project-for-symbol-at-point
      :desc "Jump to related"              "r"   #'+jg-projects-find-related
      )

;;-- zimmerframe
(map! :map jg-binding-forward-general-motion-map
      :desc "Walk Next" "z" #'zimmerframe-next
      :map jg-binding-backward-general-motion-map
      :desc "Walk Back" "z" #'zimmerframe-prev
      )

;;-- end zimmerframe

;;-- ibuffer
(map! :map jg-ibuffer-filter-map
      (:prefix "b"
       :desc "filter-by-workspace"   "w" #'ibuffer-filter-by-workspace-buffers
       :desc "filter-by-carousel" "R" #'ibuffer-filter-by-carousel-buffers
      )
      (:prefix "g"
       :desc "Group by Workspace"   "w" (ibuffer-generate! (+jg-ibuffer-generate-workspace-groups))
       )
      )
;;-- end ibuffer

;;-- carousel
(map! :map carousel-edit-map
      "C-c C-c" #'carousel-edit-commit)

(map! :map carousel-minor-mode-map
      :n "R" #'jg-workspace-run-carousel-transient
      )

(map! :map jg-binding-backward-general-motion-map
      :desc "ring window"  "r"    #'carousel-move-focus-alt
      :desc "ring oldest"  "R"    #'carousel-goto-oldest
      :desc "workspace"    "w"    #'+workspace/switch-left
      )

(map! :map jg-binding-forward-general-motion-map
      :desc "ring window"  "r"    #'carousel-move-focus
      :desc "ring newest"  "R"    #'carousel-goto-newest
      :desc "workspace"    "w"    #'+workspace/switch-right
)
;;-- end carousel

(map! :map jg-help-map
      :after jg-help-bindings
      "d p" #'+jg-projects-detect-type
  )
