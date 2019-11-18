(setq myproc nil)
(setq mytext "")
(defun myfilter (proc x)
 (message "awegaweg: %s\n" x)
 x
 )
(defun mysentinel (proc x)
 (message "sentinel fired %s %s" proc x)
  )
(let ((p (make-process
          :name "test"
          :buffer "test"
          :command '("~/.spacemacs.d/layers/subprocess/myprog.py")
          :connection-type 'pipe
          :filter 'myfilter
          :sentinel 'mysentinel
          )))
  (setq myproc p)
  (message "Started Process")
  ;; (message "%s" mytext)
  (process-send-string myproc "blah\n")
  (process-send-string myproc "bloo\n")
  (process-send-string myproc "quit\n")
  ;; (message "%s" mytext)
  (sleep-for 1)
  (message "%s" (process-status myproc))
  (message "finished")
  (if (process-live-p myproc)
      (delete-process myproc))
  (setq myproc nil)
  )
