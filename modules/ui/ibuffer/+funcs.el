;; -*- lexical-binding: t -*-

(defun +jg-ui-ibuffer-update ()
  (message "Updating ibuffer: %s" (current-time-string))
  (map! :map ibuffer-mode-map
        [normal-state] nil)

  (setq ibuffer-saved-filters jg-ui-ibuffer-filters)
  (ibuffer-clear-filter-groups)
  (ibuffer-filter-disable)

  (ibuffer-switch-to-saved-filter-groups "my-default")
  (ibuffer-switch-to-saved-filters "anti-[Helm|Magit|Help]")
  )
