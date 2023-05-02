;;; emacs/jg-vc/+bindings.el -*- lexical-binding: t; -*-

;; Clean up after magit by killing leftover magit buffers and reverting
;; affected buffers (or at least marking them as need-to-be-reverted).

(map! :leader
      :desc "Merge Mode"  "g m" #'evil-conflict-merge-state
      )

(map! :map magit-mode-map
      :nv "q" #'+magit/quit
      :nv "Q" #'+magit/quit-all

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
      )

(map! :map evil-conflict-merge-state-map
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
