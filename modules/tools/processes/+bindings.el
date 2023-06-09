;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :prefix ("rp" . "Processes")
      :desc "List-Processes" "l" #'+jg-processes-list
      :desc "Helm Processes" "h" #'helm-list-emacs-process
      :desc "Kill Preview"   "P" #'+jg-processes-kill-preview
      :desc "Process Tree"   "t" #'+jg-processes-tree
      :desc "Process Tree"   "T" #'+jg-processes-tree-all
      :desc "Launchctl List" "c" #'+jg-processes-launchctl
      :desc "Emacs"          "e" (cmd! (message "Emacs PID: %s" (emacs-pid)))
      )
