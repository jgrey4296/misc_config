
;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Timers.html
(setq mytimer nil
      mytimer-count 0
      )


(defun my-test-timer ()
  (message "My timer ran %s" mytimer-count)
  (if (> mytimer-count 10)
      (cancel-timer mytimer)
    (incf mytimer-count)
      )
  )

(progn
  (setq mytimer-count 0
        mytimer (run-at-time "5 sec" 5 'my-test-timer))

  )
