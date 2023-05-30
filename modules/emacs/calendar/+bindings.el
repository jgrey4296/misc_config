;;; +bindings.el -*- lexical-binding: t; -*-


(map! :leader
      :desc "Calendar" "oc" #'calendar
      )


(defvar jg-calendar-mode-map (make-sparse-keymap))

(map! :map jg-calendar-mode-map


      )

(map! :map calendar-mode-map
      :n "RET" #'+jg-calendar-insert-date
      )



;; (setq calendar-mode-map jg-calendar-mode-map)
