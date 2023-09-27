;;; editor/window-control/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :prefix ("w" . "Windows")
      ;; RET - workspace counsel

      :desc "Neotree Sidebar"              "s"     #'+jg-ui-tree/open
      :desc "Balance"                      "b"     #'balance-windows
      :desc "Delete Window"                "d"     #'delete-window
      :desc "Maximize"                     "m"     #'doom/window-maximize-buffer
      :desc "Undo window config"           "u"     #'winner-undo
      :desc "Redo window config"           "U"     #'winner-redo
      :desc "Window left"                  "h"     #'evil-window-left
      :desc "Window right"                 "j"     #'evil-window-down
      :desc "Window right"                 "l"     #'evil-window-right
      :desc "Window up"                    "k"     #'evil-window-up

      :desc "Split Below"                  "-"     #'split-window-below
      :desc "Split To Right"               "/"     #'split-window-right
      :desc "Shrink Horizontal"            "{"     #'shrink-window-horizontally
      :desc "Shrink Vertical"              "}"     #'shrink-window
       :desc "Toggle Layout"               "|"     #'+jg-ui-window-layout-toggle
       :desc "Rotate Windows"              "\\"    #'+jg-ui-window-rotate-forward
      )

(map! :leader
      :desc "Transient Toggle"              "T"    #'jg-toggle-main
      (:prefix "b"
       :desc "Toggle narrowing"            "-"   #'+jg-ui-toggle-narrow-buffer
       )
      (:prefix "p"
       :desc "Project Find File"            "RET" #'+jg-ui-tree/find-this-file
       )
      )

(map! :map jg-binding-vision-map
      :desc "Narrow"        "RET" #'+jg-ui-narrow-around-point
      )

;;-- highlight
(map! :map jg-binding-vision-map
      :desc "Delete Change Highlight"      "c" #'highlight-changes-remove-highlight
      :prefix ("'" . "Highlight")
       :desc  "symbol-at-point"            "." #'hi-lock-face-symbol-at-point
       :desc  "find-patterns"              "f" #'hi-lock-find-patterns
       :desc  "write-interactive-patterns" "i" #'hi-lock-write-interactive-patterns
       :desc  "lines-matching-regexp"      "l" #'hi-lock-line-face-buffer
       :desc  "phrase"                     "p" #'hi-lock-face-phrase-buffer
       :desc  "regexp"                     "r" #'hi-lock-face-buffer
       :desc  "unhighlight-regexp"         "u" #'hi-lock-unface-buffer
       :desc "changes"                     "c" #'highlight-changes-visible-mode
      )

;;-- end highlight

;;-- motion
(map! :map jg-binding-backward-general-motion-map
      :desc "Narrow"       "RET"  #'+jg-ui-narrowing-move-focus-backward
      :desc "Todo"          "t"   #'hl-todo-previous
      )

(map! :map jg-binding-forward-general-motion-map
      :desc "Narrow"       "RET"  #'+jg-ui-narrowing-move-focus-forward
      :desc "Todo"         "t"   #'hl-todo-next
)
;;-- end motion

;;-- misc
(map! :map messages-buffer-mode-map
      :after message
      :desc  "backward-word-begin"   "b"              #'evil-backward-word-begin
      :desc  "forward-word-end"      "e"              #'evil-forward-word-end
      :desc  "find-char"             "f"              #'evil-find-char
      :desc  "backward-char"         "h"              #'evil-backward-char
      :desc  "next-line"             "j"              #'evil-next-line
      :desc  "previous-line"         "k"              #'evil-previous-line
      :desc  "forward-char"          "l"              #'evil-forward-char
      :desc  "ex-search-next"        "n"              #'evil-ex-search-next
      :desc  "find-char-to"          "t"              #'evil-find-char-to
      :desc  "visual-char"           "v"              #'evil-visual-char
      :desc  "forward-word-begin"    "w"              #'evil-forward-word-begin
      )

;;-- end misc

;;-- treemacs

(defvar jg-evil-treemacs-state-map           (make-sparse-keymap))

(defvar jg-treemacs-project-map              (make-sparse-keymap))

(defvar jg-treemacs-toggle-map               (make-sparse-keymap))

(defvar jg-treemacs--fringe-indicator-bitmap (make-sparse-keymap))

(defvar jg-treemacs-workspace-map            (make-sparse-keymap))

(defvar jg-treemacs-copy-map                 (make-sparse-keymap))

(defvar jg-treemacs-node-visit-map           (make-sparse-keymap))

(defvar jg-treemacs-mode-map                 (make-sparse-keymap))

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

;;-- end treemacs

;;-- neotree

(defvar jg-neotree-mode-map (make-sparse-keymap))

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
(after! neotree
  (setq neotree-mode-map jg-neotree-mode-map)
  )

;;-- end neotree
