;;; +bindings.el -*- lexical-binding: t; -*-


(map! :leader
      :prefix ("rp" . "Processes")
      :desc "Select Process Buffer" ";"  #'+jg-processes-buffer-ivy
      :desc "List-Processes"        "l"  #'list-processes
      :desc "Kill Preview"          "P"  #'+jg-processes-kill-preview
      :desc "Process Tree"          "t"  #'+jg-processes-tree
      :desc "System Process Tree"   "T"  #'+jg-processes-tree-all
      :desc "List Threads"          "i"  #'list-threads
      :desc "Emacs PID"             "e"  (cmd! (message "Emacs PID: %s" (emacs-pid)))
      #'list-timers

      (:when (featurep :system 'macos) :desc "Launchctl List" "c" #'+jg-processes-launchctl)
      )
