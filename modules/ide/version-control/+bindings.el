;;; emacs/jg-vc/+bindings.el -*- lexical-binding: t; -*-

;; Clean up after magit by killing leftover magit buffers and reverting
;; affected buffers (or at least marking them as need-to-be-reverted).
;;-- <leader> g --- git
(map! :leader
      :prefix ("g" . "git")
      :desc "Merge Mode"  "m" #'evil-conflict-merge-state
      :desc "Git revert file"             "R"   #'vc-revert
      :desc "Git Todos"                   "T"   #'magit-todos-list
      :desc "Git revert hunk"             "r"   #'git-gutter:revert-hunk
      :desc "Git stage hunk"              "s"   #'git-gutter:stage-hunk
      :desc "Git time machine"            "t"   #'git-timemachine-toggle
      :desc "Jump to next hunk"           "n"   #'git-gutter:next-hunk
      :desc "Jump to previous hunk"       "p"   #'git-gutter:previous-hunk
      :desc "Forge dispatch"              "'"   #'forge-dispatch
      :desc "Git stage file"              "S"   #'magit-stage-file
      :desc "Git unstage file"            "U"   #'magit-unstage-file
      :desc "Magit blame"                 "B"   #'magit-blame-addition
      :desc "Magit buffer log"            "L"   #'magit-log
      :desc "Magit clone"                 "C"   #'magit-clone
      :desc "Magit dispatch"              "/"   #'magit-dispatch
      :desc "Magit fetch"                 "F"   #'magit-fetch
      :desc "Magit file delete"           "D"   #'magit-file-delete
      :desc "Magit file dispatch"         "."   #'magit-file-dispatch
      :desc "Magit status here"           "S"   #'magit-status-here
      :desc "Magit status"                "s"   #'magit-status
      :desc "Magit switch branch"         "b"   #'magit-branch-checkout
      (:prefix ("f" . "find")
       :desc "Find file"                  "f"   #'magit-find-file
       :desc "Find gitconfig file"        "g"   #'magit-find-git-config-file
       :desc "Find commit"                "c"   #'magit-show-commit
       :desc "Find issue"                 "i"   #'forge-visit-issue
       :desc "Find pull request"          "p"   #'forge-visit-pullreq)
      (:prefix ("o" . "open in browser")
       :desc "Browse file or region"      "."   #'browse-at-remote
       :desc "Browse homepage"            "h"   #'+vc/browse-at-remote-homepage
       :desc "Browse remote"              "r"   #'forge-browse-remote
       :desc "Browse commit"              "c"   #'forge-browse-commit
       :desc "Browse an issue"            "i"   #'forge-browse-issue
       :desc "Browse a pull request"      "p"   #'forge-browse-pullreq
       :desc "Browse issues"              "I"   #'forge-browse-issues
       :desc "Browse pull requests"       "P"   #'forge-browse-pullreqs)
      (:prefix ("l" . "list")
       :desc "List repositories"          "r"   #'magit-list-repositories
       :desc "List submodules"            "s"   #'magit-list-submodules
       :desc "List issues"                "i"   #'forge-list-issues
       :desc "List pull requests"         "p"   #'forge-list-pullreqs
       :desc "List notifications"         "n"   #'forge-list-notifications)
      (:prefix ("c" . "create")
       :desc "Branch"                     "b"   #'magit-branch-and-checkout
       :desc "Initialize repo"            "r"   #'magit-init
       :desc "Clone repo"                 "R"   #'magit-clone
       :desc "Commit"                     "c"   #'magit-commit-create
       :desc "Fixup"                      "f"   #'magit-commit-fixup
       :desc "Issue"                      "i"   #'forge-create-issue
       :desc "Pull request"               "p"   #'forge-create-pullreq)
      )

(map! :map magit-mode-map
      :nv "q" #'+magit/quit
      :nv "Q" #'+magit/quit-all
      :nv "z" jg-binding-vision-map

      (:when (not forge-add-default-bindings)
        [remap magit-browse-thing] #'forge-browse-dwim)

      :nv "*"  #'magit-worktree
      :nv "g=" #'magit-diff-default-context
      :nv "gi" #'forge-jump-to-issues
      :nv "gm" #'forge-jump-to-pullreqs
      :nv "]"  #'magit-section-forward-sibling
      :nv "["  #'magit-section-backward-sibling
      :nv "gr" #'magit-refresh
      :nv "gR" #'magit-refresh-all
      :m "]d"  #'+vc-gutter/next-hunk
      :m "[d"  #'+vc-gutter/previous-hunk
      )

;; Close transient with ESC
(map! :map transient-map
      [escape] #'transient-quit-one
      )

(map! :map magit-todos-section-map
      "j" nil
      )

;; q is enough; ESC is way too easy for a vimmer to accidentally press,
;; especially when traversing modes in magit buffers.
(map! :map magit-status-mode-map
      :after magit-status
      :n [escape] nil
      :n [tab] #'magit-section-toggle
      :nv "gz" #'magit-refresh
      :localleader
      :desc "Insert Tag"  "i" #'+jg-vcs-insert-tag
      )

(map! :map git-commit-mode-map
      :after git-commit
      :localleader
      :desc "Insert Tag"  "i" #'+jg-vcs-insert-tag
      )

(map! :map (magit-stash-mode-map
            magit-revision-mode-map
            magit-process-mode-map
            magit-diff-mode-map)
      :n [tab] #'magit-section-toggle
      )

(map! :map magit-process-mode-map
      :n "`" #'quit-window
      )

(map! :map magit-remote-section-map
      :when (not forge-add-default-bindings)
      [remap magit-browse-thing] #'forge-browse-remote
      )

(map! :map magit-branch-section-map
      :when (not forge-add-default-bindings)
      [remap magit-browse-thing] #'forge-browse-branch
      )

(map! :map magit-diff-mode-map
      :nv "gd" #'magit-jump-to-diffstat-or-diff
      )

(map! :map git-timemachine-mode-map ;; Git Timemachine
      :after git-timemachine
      :n "[ g" #'git-timemachine-show-previous-revision
      :n "] g" #'git-timemachine-show-next-revision

      :n "gb"  #'git-timemachine-blame
      :n "gtc" #'git-timemachine-show-commit
      )

(map! :map evil-conflict-merge-state-map
      :after conflict-merge-state
      :desc "Next Conflict"  "j"   #'smerge-next
      :desc "Prev Conflict"  "k"   #'smerge-prev
      :desc "Choose Upper"   "U"   #'smerge-keep-upper
      :desc "Choose Lower"   "L"   #'smerge-keep-lower
      :desc "Choose"         "RET" #'smerge-keep-current
      doom-leader-key doom-leader-map
      )

;; All forge list modes are derived from `forge-topic-list-mode'
(map! :map forge-topic-list-mode-map
      :when (modulep! +forge)
      :n "q" #'kill-current-buffer
      )

(map! :map code-review-mode-map
      :after code-review
      :n "r" #'code-review-transient-api
      :n "RET" #'code-review-comment-add-or-edit
      )

;; A more intuitive behavior for TAB in magit buffers:
(after! git-rebase
  (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
    (when-let (desc (assoc (car key) evil-collection-magit-rebase-commands-w-descriptions))
      (setcar desc (cdr key)))))

(map! :map  git-rebase-mode-map
      :n "gj" #'git-rebase-move-line-down
      :n "gk" #'git-rebase-move-line-up
      )

(after! magit-section
  ;; These numbered keys mask the numerical prefix keys. Since they've already
  ;; been replaced with z1, z2, z3, etc (and 0 with g=), there's no need to
  ;; keep them around:
  (undefine-key! magit-section-mode-map "M-1" "M-2" "M-3" "M-4" "1" "2" "3" "4" "0")
  )
;; `evil-collection-magit-section' binds these redundant keys.
(map! :map magit-section-mode-map
      :after magit-section
      :n "1" nil
      :n "2" nil
      :n "3" nil
      :n "4" nil
      )
