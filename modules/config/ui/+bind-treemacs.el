;;; +bind-treemacs.el -*- lexical-binding: t; no-byte-compile: t; -*-

(map! :map jg-evil-treemacs-state-map
      ;; "."  #'treemacs-root-down
      ;; ","  #'treemacs-root-up
      "H"                  #'treemacs-collapse-parent-node
      "h"                  #'treemacs-COLLAPSE-action
      "j"                  #'treemacs-next-line
      "k"                  #'treemacs-previous-line
      "l"                  #'+jg-ui-treemacs-expand-dir

      [tab]                #'treemacs-TAB-action
      [?\t]                #'treemacs-TAB-action
      [return]             #'treemacs-RET-action

      (:prefix ("c" . "Create")
      "f"                 #'treemacs-create-file
      "d"                 #'treemacs-create-dir
      )
      ;; "?"               #'treemacs-common-helpful-hydra
      "RET"                #'treemacs-RET-action
      "r"                  #'treemacs-refresh
      "d"                  #'treemacs-delete-file
      "R"                  #'treemacs-rename-file
      "u"                  #'treemacs-goto-parent-node
      "q"                  #'treemacs-quit
      "Q"                  #'treemacs-kill-buffer
      "P"                  #'treemacs-peek-mode
      "n"                  #'treemacs-next-line
      "p"                  #'treemacs-previous-line
      "w"                  #'treemacs-set-width
      "<"                  #'treemacs-decrease-width
      ">"                  #'treemacs-increase-width
      "m"                  #'treemacs-move-file
      "g"                  #'treemacs-refresh
      "s"                  #'treemacs-resort
      "b"                  #'treemacs-add-bookmark
      "!"                  #'treemacs-run-shell-command-for-current-node
      "C"                  #'treemacs-cleanup-litter
      "="                  #'treemacs-fit-window-width
      "W"                  #'treemacs-extra-wide-toggle
      )

(map! :map jg-evil-treemacs-state-map
      :prefix ("o" . "Node Visiting")
      "v"  #'treemacs-visit-node-vertical-split
      "c"  #'treemacs-visit-node-close-treemacs
      "h"  #'treemacs-visit-node-horizontal-split
      "o"  #'treemacs-visit-node-no-split
      "aa" #'treemacs-visit-node-ace
      "ah" #'treemacs-visit-node-ace-horizontal-split
      "av" #'treemacs-visit-node-ace-vertical-split
      "r"  #'treemacs-visit-node-in-most-recently-used-window
      "x"  #'treemacs-visit-node-in-external-application
      )

(map! :map jg-evil-treemacs-state-map
      :prefix ("t" . "Toggles")
      "h" #'treemacs-toggle-show-dotfiles
      "i" #'treemacs-hide-gitignored-files-mode
      "w" #'treemacs-toggle-fixed-width
      "v" #'treemacs-fringe-indicator-mode
      "g" #'treemacs-git-mode
      "f" #'treemacs-follow-mode
      "a" #'treemacs-filewatch-mode
      "n" #'treemacs-indent-guide-mode
      "c" #'treemacs-indicate-top-scroll-mode
      "d" #'treemacs-git-commit-diff-mode
     )

(map! :map jg-evil-treemacs-state-map
      :prefix ("y" . "Copying")
      "a" #'treemacs-copy-absolute-path-at-point
      "r" #'treemacs-copy-relative-path-at-point
      "p" #'treemacs-copy-project-path-at-point
      "f" #'treemacs-copy-file
      "v" #'treemacs-paste-dir-at-point-to-minibuffer
      )

(after! (treemacs evil-treemacs)
  (setq evil-treemacs-state-map           jg-evil-treemacs-state-map
        treemacs-mode-map                 jg-treemacs-mode-map
        treemacs-project-map              jg-treemacs-project-map
        treemacs-toggle-map               jg-treemacs-toggle-map
        treemacs--fringe-indicator-bitmap jg-treemacs--fringe-indicator-bitmap
        treemacs-workspace-map            jg-treemacs-workspace-map
        treemacs-copy-map                 jg-treemacs-copy-map
        treemacs-node-visit-map           jg-treemacs-node-visit-map
        )
  )

;;; +bind-treemacs.el ends here
