;;; +bindings.el -*- lexical-binding: t; -*-


(map! :leader
      :prefix ("p" . "Processes")
      :desc "Select Process Buffer" ";"  #'+jg-processes-buffer-ivy
      :desc "Kill Preview"          "P"  #'+jg-processes-kill-preview
      :desc "Emacs PID"             "e"  #'+jg-print-pid

      (:prefix ("l" . "List")
       :desc "List-Processes"        "p"  #'list-processes
       :desc "List Threads"          "h"  #'list-threads
       :desc "List Timers"           "t"  #'list-timers
       :desc "Emacs Process"         "e"  #'+jg-processes-tree
       :desc "System Tree"           "s"  #'+jg-processes-tree-all
       )
      )
