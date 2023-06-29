;;; +bindings.el -*- lexical-binding: t; -*-

(defvar jg-neotree-mode-map (make-sparse-keymap))

;; Delete the current workspace if closing the last open window

(define-key! persp-mode-map
  [remap delete-window] #'+workspace/close-window-or-workspace
  [remap evil-window-delete] #'+workspace/close-window-or-workspace)

(map! :leader
      :desc "Window Hydra"       "W" #'hydra-workspace/body
      :desc "Ring Hydra"         "R" #'hydra-window-ring/body
      :desc "Workspace Counsel"  "w RET" #'+jg-workspaces-ivy
      :n "p RET" #'+neotree/find-this-file
      )

(map! :leader
      :prefix ("w" . "Windows")
      :desc "Neotree Sidebar"              "s" #'+neotree/open
      :desc "Delete workspace"             "DEL" #'+workspace/delete
      :desc "Most Recent"                  "L" #'window-ring-goto-most-recent
      :desc "Oldest"                       "H" #'window-ring-goto-oldest
      )

(map! :leader
      :prefix ("p" . "project")
      "?" #'+jg-projects-detect-type
      :desc "run cmd in project root"      "!"  #'projectile-run-shell-command-in-root
      :desc "project root"                 "`"  (cmd! (find-file (doom-project-root)))
      :desc "root shell"                   "'"  #'projectile-run-shell
      :desc "search project for symbol"    "."  #'+default/search-project-for-symbol-at-point

      :desc "compile in project"           "c"  #'projectile-compile-project
      :desc "open project scratch buffer"  "x"  #'+jg-ui-open-scratch-buffer

      :desc "find file in project"         "f"  #'projectile-find-file
      :desc "find other file"              "o"  #'+jg-projects-find-related

      ;; later expanded by projectile
      (:prefix ("4" . "in other window"))
      (:prefix ("5" . "in other frame"))
      )

;;-- management
(map! :leader
      :prefix ("p m" . "management")
       :desc "invalidate project cache"     "c"  #'projectile-invalidate-cache
       :desc "browse other project"         ">"  #'doom/browse-in-other-project
       :desc "add new project"              "a"  #'projectile-add-known-project
       :desc "discover projects in folder"  "d"  #'+default/discover-projects
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

(map! :map jg-binding-forward-general-motion-map
      :desc "Walk Next" "n" #'project-walk-next
      :map jg-binding-backward-general-motion-map
      :desc "Walk Back" "n" #'project-walk-prev
      )

;;-- end project walk

;;-- ibuffer
(map! :map jg-ibuffer-filter-map
      (:prefix "b"
       :desc "filter-by-workspace"   "w" #'ibuffer-filter-by-workspace-buffers
       :desc "filter-by-window-ring" "R" #'ibuffer-filter-by-window-ring-buffers
      )
      (:prefix "g"
       :desc "Group by Workspace"   "w" (ibuffer-generate! (+jg-ibuffer-generate-workspace-groups))
       )
      )
;;-- end ibuffer

;;-- neotree
(map! :map jg-neotree-mode-map
      :after neotree
      :n "v"   (neotree-make-executor :file-fn 'neo-open-file-vertical-split)

      :n "i"  #'ignore
      :n "g"  #'neotree-refresh
      :n "q"  #'neotree-hide
      :n "Q"  (cmd! (kill-buffer (current-buffer)))
      :n "."  #'neotree-hidden-file-toggle
      :n "\\" #'neotree-change-root
      :n "r"  #'neotree-rename-node

      :n "h"  #'+neotree/collapse-or-up
      :n "l"  #'+neotree/expand-or-open
      :n "H"  #'neotree-select-up-node
      :n "L"  #'neotree-select-down-node
      :n "n"  #'neotree-select-next-sibling-node
      :n "N"  #'neotree-select-previous-sibling-node

      :n "RET" (neotree-make-executor :file-fn 'neo-open-file :dir-fn  'neo-open-dir)
      )

(setq neotree-mode-map jg-neotree-mode-map)
;;-- end neotree

;;-- window-ring
(map! :map window-ring-edit-map
      "C-c C-c" #'window-ring-edit-commit)

(map! :map jg-binding-backward-general-motion-map
      :desc "ring window"  "r"    #'window-ring-move-focus-alt
      :desc "ring oldest"  "R"    #'window-ring-goto-oldest
      :desc "workspace"    "w"    #'+workspace/switch-left
      )

(map! :map jg-binding-forward-general-motion-map
      :desc "ring window"  "r"    #'window-ring-move-focus
      :desc "ring newest"  "R"    #'window-ring-goto-newest
      :desc "workspace"    "w"    #'+workspace/switch-right
)
;;-- end window-ring

(map! :map jg-help-map
      :after jg-help-bindings
      "d p" #'+jg-projects-detect-type
  )
