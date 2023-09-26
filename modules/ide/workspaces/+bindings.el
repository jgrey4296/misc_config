;;; +bindings.el -*- lexical-binding: t; -*-

(defvar jg-neotree-mode-map (make-sparse-keymap))


;; Delete the current workspace if closing the last open window

(define-key! persp-mode-map
  [remap delete-window] #'+workspace/close-window-or-workspace
  [remap evil-window-delete] #'+workspace/close-window-or-workspace)

(map! :leader
      :desc "Window Control"           "W"     #'transient-workspace
      :desc "Carousel Control"         "R"     #'transient-carousel
      :desc "Workspace Counsel"        "w RET" #'+jg-workspaces-ivy
      (:prefix ("w" . "Windows")
       :desc "Neotree Sidebar"              "s"   #'+neotree/open
       :desc "Delete workspace"             "DEL" #'+workspace/delete
       )
      (:prefix "c"
       :desc "compile in project"           "c"  #'projectile-compile-project
       )

      )

(map! :leader
      :prefix ("p" . "project")
      :desc "Project Find File"            "RET" #'+neotree/find-this-file
      :desc "run cmd in project root"      "!"   #'projectile-run-shell-command-in-root
      :desc "root shell"                   "'"   #'projectile-run-shell
      :desc "search project for symbol"    "."   #'+default/search-project-for-symbol-at-point

      :desc "compile in project"           "c"   #'projectile-compile-project
      :desc "open project scratch buffer"  "x"   #'+jg-ui-open-scratch-buffer

      :desc "find file in project"         "f"   #'projectile-find-file
      :desc "find other file"              "o"   #'+jg-projects-find-related

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

;;-- carousel
(map! :map carousel-edit-map
      "C-c C-c" #'carousel-edit-commit)

(map! :map carousel-minor-mode-map
      :n "R" #'transient-carousel
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
