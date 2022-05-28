;;; util/jg-misc/+bindings.el -*- lexical-binding: t; -*-
(evil-make-intercept-map messages-buffer-mode-map)

(map! :leader
      :desc "Have you Played?" "o h h" #'+jg-misc-helm-rps-have-you-playeds
      )

(map! :map help-map
      "DEL" #'free-keys
      )

(map! :map (sh-mode-map shell-mode-map)
      :localleader
      :desc "Docs: Brew"  "1" (cmd! (+jg-browse-url "https://brew.sh/"))
      :desc "Docs: Awk"   "2" (cmd! (+jg-browse-url "https://www.gnu.org/software/gawk/manual/gawk.html"))
      )

(map! :mode vlf-mode
      "] A" 'vlf-next-batch-from-point
      "] a" 'vlf-next-batch
      "[ a" 'vlf-prev-batch
      "SPC a U v " 'vlf-set-batch-size
      )

(map! :leader
      :prefix "t"
      :desc "Semantic" "S" #'semantic-mode
      )

(defun +jg-misc-free-key-binding-update ()
  (map! :map free-keys-mode-map
        :desc "Change Buffer" :n "b" #'free-keys-change-buffer
        :desc "Revert Buffer" :n "g" #'revert-buffer
        :desc "Describe Mode" :n "h" #'describe-mode
        :desc "Set Prefix"    :n "p" #'free-keys-set-prefix
        :desc "Quit"          :n "q" #'quit-window
        )
  (evil-make-intercept-map free-keys-mode-map)
  )

(map! :map semantic-mode-map
      :localleader
      :prefix ("^" . "Semantic")
      (:prefix ("t" . "toggle")
       :desc "Stick-func"     "s" #'semantic-stickyfunc-mode
       :desc "Highlight-func" "h" #'semantic-highlight-func-mode
       )

      )

(map! :map rst-mode-map
      :localleader
      :desc "Reference" "1" (cmd! (+jg-browse-url "https://restructuredtext.documatt.com/element/rubric.html"))
      )

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
