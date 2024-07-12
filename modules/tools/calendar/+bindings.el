;;; +bindings.el -*- lexical-binding: t; -*-

(evil-make-overriding-map jg-calendar-mode-map)

(map! :leader
      :desc "Calendar" "oc" #'calendar
      )

(map! :map jg-calendar-mode-map
      :n "RET" #'+jg-calendar-insert-date
      :n "TAB" #'diary-view-entries
      :n "q" #'calendar-exit

      ;; Viewing
      :n "o" #'diary-view-other-diary-entries
      :n "m" #'diary-mark-entries
      :n "u" #'calendar-unmark
      :n "d" #'diary-view-entries
      :n "s" #'diary-show-all-entries
      :n "S" #'calendar-sunrise-sunset
      :n "M" #'calendar-lunar-phases
      :n "p" (cmd! (let ((date (calendar-day-of-year-string (calendar-cursor-to-date t)))) (message "Date: %s" date) (kill-new date)))

      :n "P" (cmd! (let ((date (calendar-julian-date-string (calendar-cursor-to-date t)))) (message "Date: %s" date) (kill-new date)))

      ;; Movement
      :n "." #'calendar-goto-today
      :n "/" #'calendar-goto-date
      :n "l" #'calendar-forward-day
      :n "h" #'calendar-backward-day
      :n "j" #'calendar-forward-week
      :n "k" #'calendar-backward-week
      :n "J" #'calendar-forward-month
      :n "K" #'calendar-backward-month
      )

(map! :map jg-calendar-mode-map
      (:prefix ("i" . "Insert")
       :desc "Insert Entry"         :n "e" #'diary-insert-entry
       :desc "Insert Weekly Entry"  :n "w" #'diary-insert-weekly-entry
       :desc "Insert Monthly Entry" :n "m" #'diary-insert-monthly-entry
       :desc "Insert Yearly Entry"  :n "y" #'diary-insert-yearly-entry
       )

      (:prefix ("e" . "Export")
       :desc "Html Month" :n "m" #'cal-html-cursor-month
       :desc "Html Year"  :n "y" #'cal-html-cursor-year
       )
      )

(map! :map jg-calendar-mode-map
      :localleader
      ;; Viewing
      :n "o" #'diary-view-other-diary-entries
      :n "m" #'diary-mark-entries
      :n "u" #'calendar-unmark
      :n "d" #'diary-view-entries
      :n "s" #'diary-show-all-entries
      :n "S" #'calendar-sunrise-sunset
      :n "M" #'calendar-lunar-phases
      :n "p" (cmd! (let ((date (calendar-day-of-year-string (calendar-cursor-to-date t)))) (message "Date: %s" date) (kill-new date)))

      :n "P" (cmd! (let ((date (calendar-julian-date-string (calendar-cursor-to-date t)))) (message "Date: %s" date) (kill-new date)))

      ;; Movement
      :n "." #'calendar-goto-today
      :n "/" #'calendar-goto-date

      (:prefix ("i" . "Insert")
       :desc "Insert Entry"         :n "e" #'diary-insert-entry
       :desc "Insert Weekly Entry"  :n "w" #'diary-insert-weekly-entry
       :desc "Insert Monthly Entry" :n "m" #'diary-insert-monthly-entry
       :desc "Insert Yearly Entry"  :n "y" #'diary-insert-yearly-entry
       )

      (:prefix ("e" . "Export")
       :desc "Html Month" :n "m" #'cal-html-cursor-month
       :desc "Html Year"  :n "y" #'cal-html-cursor-year
       )
      )

(setq calendar-mode-map jg-calendar-mode-map)
