;;; lang/org/+bindings.el -*- lexical-binding: t; -*-
(message "Loading modules/lang/org/+bindings.el")

(map! :after org-agenda
      :map org-agenda-mode-map
      :m "C-SPC" #'org-agenda-show-and-scroll-up
      :localleader
      (:prefix ("d" . "date/deadline")
       :desc "Deadline" "d" #'org-agenda-deadline
       :desc "Schedule" "s" #'org-agenda-schedule)
      (:prefix ("c" . "clock")
      :desc "Clock: Cancel" "c" #'org-agenda-clock-cancel
       :desc "Clock: Goto" "g" #'org-agenda-clock-goto
       :desc "ClockIn" "i" #'org-agenda-clock-in
       :desc "ClockOut" "o" #'org-agenda-clock-out
       :desc "Report Mode" "r" #'org-agenda-clockreport-mode
       :desc "Show Issues" "s" #'org-agenda-show-clocking-issues)
      :desc "Set Tags" "q" #'org-agenda-set-tags
      :desc "Refile" "r" #'org-agenda-refile
      :desc "Todo" "t" #'org-agenda-todo)

(map! :after org-agenda
      :map org-agenda-mode-map
      :localleader
      :prefix "v"
      "w"   #'org-agenda-week-view
      "m"   #'org-agenda-month-view
      )

(map! :after evil-org
      :map evil-org-mode-map
      :ni [C-return]   #'+org/insert-item-below
      :ni [C-S-return] #'+org/insert-item-above
      ;; navigate table cells (from insert-mode)
      :i  "C-l"     (cmds! (org-at-table-p) #'org-table-next-field
                           #'org-end-of-line)
      :i  "C-h"     (cmds! (org-at-table-p) #'org-table-previous-field
                           #'org-beginning-of-line)
      :i  "C-k"     (cmds! (org-at-table-p) #'+org/table-previous-row
                           #'org-up-element)
      :i  "C-j"     (cmds! (org-at-table-p) #'org-table-next-row
                           #'org-down-element)
      :ni "C-S-l"   #'org-shiftright
      :ni "C-S-h"   #'org-shiftleft
      :ni "C-S-k"   #'org-shiftup
      :ni "C-S-j"   #'org-shiftdown

      ;;:n "TAB" #'org-cycle-internal-local
      ;; more intuitive RET keybinds
      :n [return]   #'+org/dwim-at-point
      :n "RET"      #'+org/dwim-at-point
      :i [return]   (cmd! (org-return electric-indent-mode))
      :i "RET"      (cmd! (org-return electric-indent-mode))
      :i [S-return] #'+org/shift-return
      :i "S-RET"    #'+org/shift-return
      ;; more vim-esque org motion keys (not covered by evil-org-mode)
      :m "]h"  #'org-forward-heading-same-level
      :m "[h"  #'org-backward-heading-same-level
      :m "]l"  #'org-next-link
      :m "[l"  #'org-previous-link
      :m "]c"  #'org-babel-next-src-block
      :m "[c"  #'org-babel-previous-src-block
      :n "gQ"  #'org-fill-paragraph
      ;; sensible vim-esque folding keybinds
      :n "za"  #'+org/toggle-fold
      :n "zA"  #'org-shifttab
      :n "zc"  #'+org/close-fold
      :n "zC"  #'outline-hide-subtree
      :n "zm"  #'+org/hide-next-fold-level
      :n "zM"  #'+org/close-all-folds
      :n "zn"  #'org-tree-to-indirect-buffer
      :n "zo"  #'+org/open-fold
      :n "zO"  #'outline-show-subtree
      :n "zr"  #'+org/show-next-fold-level
      :n "zR"  #'+org/open-all-folds
      :n "zi"  #'org-toggle-inline-images

      :map org-read-date-minibuffer-local-map
      "C-h"   (cmd! (org-eval-in-calendar '(calendar-backward-day 1)))
      "C-l"   (cmd! (org-eval-in-calendar '(calendar-forward-day 1)))
      "C-k"   (cmd! (org-eval-in-calendar '(calendar-backward-week 1)))
      "C-j"   (cmd! (org-eval-in-calendar '(calendar-forward-week 1)))
      "C-S-h" (cmd! (org-eval-in-calendar '(calendar-backward-month 1)))
      "C-S-l" (cmd! (org-eval-in-calendar '(calendar-forward-month 1)))
      "C-S-k" (cmd! (org-eval-in-calendar '(calendar-backward-year 1)))
      "C-S-j" (cmd! (org-eval-in-calendar '(calendar-forward-year 1))))
