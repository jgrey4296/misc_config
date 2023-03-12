;;; util/jg-misc/+bindings.el -*- lexical-binding: t; -*-
(evil-make-intercept-map messages-buffer-mode-map)

(map! :map jg-binding-help-map
      "DEL" #'free-keys
      )

(map! :mode vlf-mode
      :after vlf-mode
      "] A" 'vlf-next-batch-from-point
      "] a" 'vlf-next-batch
      "[ a" 'vlf-prev-batch
      "SPC a U v " 'vlf-set-batch-size
      )

(map! :map free-keys-mode-map
      :after free-keys
      :desc "Change Buffer" :n "b" #'free-keys-change-buffer
      :desc "Revert Buffer" :n "g" #'revert-buffer
      :desc "Describe Mode" :n "h" #'describe-mode
      :desc "Set Prefix"    :n "p" #'free-keys-set-prefix
      :desc "Quit"          :n "q" #'quit-window
      )

(map! :map semantic-mode-map
      :after semantic
      :localleader
      :prefix ("^" . "Semantic")
      (:prefix ("t" . "toggle")
       :desc "Stick-func"     "s" #'semantic-stickyfunc-mode
       :desc "Highlight-func" "h" #'semantic-highlight-func-mode
       )

      )

(after! calendar
  (setq calendar-mode-map (make-sparse-keymap))
  (evil-make-overriding-map calendar-mode-map)

  (map! :map calendar-mode-map
        ;; General
        :n "q" #'calendar-exit

        ;; Viewing
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

  )

(map! :leader
      :prefix "t"
       :desc "Semantic" "S" #'semantic-mode
      )
