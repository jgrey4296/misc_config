(when (featurep! :domain-specific twitter)

  (defun +jg-twitter-open_user ()
    (interactive)
    ;;get word at point
     (org-open-link-from-string (format "[[https://www.twitter.com/%s]]" (current-word)))
    )

  (defun +jg-twitter-tweet ()
    """ Creates a window to write a tweet in,
        if already in that window, tweet it
     """
    (interactive)
    (if (string-equal (buffer-name) +jg-twitter-tweet_buff_name)
        (+jg-twitter-twitter-tweet)
      (progn
        (evil-window-new (get-buffer-window (current-buffer))
                         +jg-twitter-tweet_buff_name)
        (set (make-local-variable 'backup-inhibited) t)
        (auto-save-mode -1)
        (evil-window-set-height 10)
        (redraw-display)
        )
      )
    )

  (defun +jg-twitter-twitter-tweet ()
    """ Actually tweet """
    (interactive)
    (+jg-twitter-twitter-tweet-text (buffer-string) (buffer-local-variables) '(+jg-twitter-tweet_sentinel))
    )

  (defun +jg-twitter-twitter-tweet-text (text &optional locals sentinels)
    (let ((cmd (format "status=%s" text)))
      (if (assq 'media_id locals)
          (setq cmd (format "%s&media_ids=%s" cmd (cdr (assq 'media_id locals)))))

      (start-process +jg-twitter-twurl_proc_name +jg-twitter-twurl_buff_name
                     +jg-twitter-twurl_command_name  "-d" (format "%s" cmd) "-X" "POST" "-H"
                     +jg-twitter-twurl_default_host
                     +jg-twitter-twurl_tweet)
      (if sentinels
          (-map (lambda (sent)
                  (set-process-sentinel
                   (get-process +jg-twitter-twurl_proc_name)
                   sent))
                sentinels)
        )
    ))

  (defun +jg-twitter-twitter-add-picture ()
    """ start the process of uploading twitter media_id
        to the local-variables of the current buffer """
    (interactive)
    (if (not (boundp '+jg-twitter-twitter-image-helm-source))
        (message "Use Helm First")
      (helm :sources +jg-twitter-twitter-image-helm-source
            :input "~/Desktop/"))
  )

  (defun +jg-twitter-twitter-upload-image (candidate)
    """ Initialise the upload """
    (let* ((sz (f-size candidate))
           (cmd (format "command=%s&total_bytes=%s&media_type=image/png" "INIT" sz))
           (target-buffer (buffer-name))
           )
      (if (> sz 5242880) (message "File too large")
        (progn
          (setq +jg-twitter-twurl_file candidate
                +jg-twitter-twurl_target_tweet target-buffer)
          (with-current-buffer (get-buffer-create +jg-twitter-twurl_buff_name)
            (erase-buffer)
            (beginning-of-buffer)
            (start-process +jg-twitter-twurl_proc_name +jg-twitter-twurl_buff_name
                           "twurl" "-H" +jg-twitter-twurl_media_host
                           +jg-twitter-twurl_upload "-d" cmd)
            (set-process-sentinel (get-process +jg-twitter-twurl_proc_name) '+jg-twitter-sentinel_upload)
            )
          )
        )
      )
    )

  (defun +jg-twitter-sentinel_upload (&rest args)
    """ Append data """
    (let* ((buf-json (with-current-buffer +jg-twitter-twurl_buff_name
                       (beginning-of-buffer) (json-read)))
           (media_str (cdr (assq 'media_id_string buf-json)))
           (cmd (format "command=%s&media_id=%s&segment_index=0" "APPEND" media_str))
           )
      (setq +jg-twitter-twurl_media_id media_str)
      (start-process +jg-twitter-twurl_proc_name +jg-twitter-twurl_buff_name
                     "twurl" "-H" +jg-twitter-twurl_media_host +jg-twitter-twurl_upload "-d" cmd "--file" +jg-twitter-twurl_file "--file-field" "media")
      (set-process-sentinel (get-process +jg-twitter-twurl_proc_name) '+jg-twitter-sentinel_final)
      )
    )

  (defun +jg-twitter-sentinel_final (&rest args)
    """ Finalize the upload """
    (let* ((cmd (format "command=%s&media_id=%s" "FINALIZE" +jg-twitter-twurl_media_id)))
      (start-process +jg-twitter-twurl_proc_name +jg-twitter-twurl_buff_name
                     "twurl" "-H" +jg-twitter-twurl_media_host +jg-twitter-twurl_upload "-d" cmd)
      (set-process-sentinel (get-process +jg-twitter-twurl_proc_name) '+jg-twitter-add_media_id_to_tweet)
      )
    )

  (defun +jg-twitter-add_media_id_to_tweet (&rest args)
    """ Actually insert the media_id to the local vars of the tweet buffer """
    (with-current-buffer +jg-twitter-twurl_target_tweet
      (make-local-variable 'media_id)
      (setq media_id +jg-twitter-twurl_media_id)
      )
    (message "Media Inserted")
    )

  (defun +jg-twitter-tweet_sentinel (&rest args)
    """ Cleanup after tweeting """
    (message (format "Tweeted %s" args))
    (set-buffer-modified-p nil)
    (kill-buffer-and-window)
    (redraw-display)
    )
  )