(setq mytimer nil
      mytimer-count 0
      myproc nil
      ;; myprogram (expand-file-name "~/.spacemacs.d/lisp/mytimerprog.py")
      myprogram (expand-file-name "~/.spacemacs.d/lisp/stdio_test.py")
      )

(defun myfilter (proc x)
  (message "Response: %s\n" x)
  (if (string-match "quit" x)
      (setq mytimer-count 1)
    )
  x
  )

(defun mysentinel (proc x)
  (message "Sentinel: %s %s" proc x)
  x
  )

(defun mytimer-listen ()
  (message "Timer Listen")
  (cond
   ((process-live-p myproc)
    (progn (message "Process active")))
           ;; (accept-process-output myproc 0.1 nil t)))
   ((not (eq 0 mytimer-count))
    (progn
      (cancel-timer mytimer)
      (if (process-live-p myproc)
          (quit-process myproc))
      ))
   (t (cancel-timer mytimer))
   )
  )

(progn
  (setq myproc (make-process
                :name "test"
                :buffer "test"
                :command (list "python3" myprogram)
                :connection-type 'pipe
                :filter 'myfilter
                :sentinel 'mysentinel
                )
        ;; mytimer (run-at-time "5 sec" 5 'mytimer-listen)
        mytimer-count 0
        )

  (message "Starting Process: %s %s" myproc (process-status myproc))
  ;; (continue-process myproc)
  (process-send-string myproc "bloo\n")
  (message "Post Send: %s" (process-status myproc))
  )

 (process-send-string myproc "quit\n")
