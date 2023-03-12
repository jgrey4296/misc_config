;;; emacs/jg-vc/+bindings.el -*- lexical-binding: t; -*-

 (evil-define-state conflict-merge
    "Merge Conflict State."
    :tag "<MC>"
    :message "-- MERGE CONFLICT --"
    ;; :enable (motion)
    ;; :input-method t
    :suppress-keymap t
    )

(map! :map evil-conflict-merge-state-map
      :desc "Next Conflict"  "j"   #'smerge-next
      :desc "Prev Conflict"  "k"   #'smerge-prev
      :desc "Choose Upper"   "U"   #'smerge-keep-upper
      :desc "Choose Lower"   "L"   #'smerge-keep-lower
      :desc "Choose"         "RET" #'smerge-keep-current
      doom-leader-key doom-leader-map
      )

(evil-make-intercept-map evil-conflict-merge-state-map)

(map! :after (git-commit magit-status)
      :map (git-commit-mode-map magit-status-mode)
      :localleader
      :desc "Git Docs"    "1" (cmd! (browse-url "https://git-scm.com/doc"))
      :desc "Insert Tag"  "i" #'+jg-vcs-insert-tag
      )
(map! :after jg-bindings-total
      :leader
      :prefix "p"
      :desc "Gradle -q" :n "g" #'+jg-vcs-run-gradle-quiet
      :desc "Gradle"    :n "G" #'+jg-vcs-run-gradle
      )


;;-- git timemachine
;; Git Timemachine
(map! :after git-timemachine
      :map git-timemachine-mode-map
      :n "[ g" #'git-timemachine-show-previous-revision
      :n "] g" #'git-timemachine-show-next-revision
      )

;;-- end git timemachine
