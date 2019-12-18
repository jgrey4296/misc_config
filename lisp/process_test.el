(setq myproc nil)
(setq mytext "")
(defun myfilter (proc x)
  (message "My filter")
  (message "awegaweg: %s\n" x)
  nil
  )
(defun mysentinel (proc x)
  (message "sentinel fired %s %s" proc x)
  )
(let ((p (make-process
          :name "test"
          :buffer "test"
          :command (list "python3" (expand-file-name "~/.spacemacs.d/lisp/myprog.py"))
          :connection-type 'pipe
          :filter 'myfilter
          :sentinel 'mysentinel
          )))
  (setq myproc p)
  (message "Started Process")
  (message "%s" mytext)
  (stop-process p)
  (continue-process p)
  (process-send-string myproc "blah\n")
  (accept-process-output p)
  (process-send-string myproc "bloo\n")
  (accept-process-output p)
  (process-send-string myproc "quit\n")
  (message "%s" mytext)
  (sleep-for 1)
  (message "%s" (process-status myproc))
  (message "finished")
  (if (process-live-p myproc)
      (delete-process myproc))
  (setq myproc nil)
  )
