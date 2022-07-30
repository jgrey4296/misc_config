;;; domain-specific/twitter/+downloader.el -*- lexical-binding: t; -*-

(defun +jg-twitter-downloader (id_str)
  " Download a minibuffer supplied tweet id "
  (interactive "MDownload:\n")
  ;; Start a process
  (let* ((mv-cmd (format "cd %s" jg-twitter-download-repo))
         (download-cmd (format "%s --tweet %s" jg-twitter-download-py id_str))
         (total-cmd (s-join "; " (list  mv-cmd download-cmd)))
         (process (start-process "Tweet Downloader" "Tweet-Download"
                                 "bash" "-c" total-cmd))
         (sentinel #'(lambda (proc chng)
                       (if (s-equals? (s-trim chng) "finished")
                           (progn (message "Tweet Download Finished")
                                  (find-file-other-window (f-join jg-twitter-download-repo
                                                                  ".temp_download"
                                                                  "orgs")))
                         (message "Tweet Download: %s" chng)
                         )))
         )
    (set-process-sentinel process sentinel)
    )
  )
