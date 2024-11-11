;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :prefix ("rp" . "Processes")
      :desc "List-Processes" "l" #'list-processes
      :desc" List-Processes" "h" #'list-processes
      :desc "Kill Preview"   "P" #'+jg-processes-kill-preview
      :desc "Process Tree"   "t" #'+jg-processes-tree
      :desc "Process Tree"   "T" #'+jg-processes-tree-all
      (:when (featurep :system 'macos)
        :desc "Launchctl List" "c" #'+jg-processes-launchctl
        )
      :desc "Emacs PID"        "e" (cmd! (message "Emacs PID: %s" (emacs-pid)))
      )
