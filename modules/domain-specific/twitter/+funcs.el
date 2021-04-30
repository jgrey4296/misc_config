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
  (let ((selection (if (eq evil-state 'visual)
                       (buffer-substring-no-properties
                        evil-visual-beginning
                        evil-visual-end))))
    (+jg-twitter-tweet-with-input selection)
    )
)

(defun +jg-twitter-tweet-reply ()
  """ Creates a window to write a tweet in,
        if already in that window, tweet it
     """
  (interactive)
  (let ((selection (if (eq evil-state 'visual)
                       (buffer-substring-no-properties
                        evil-visual-beginning
                        evil-visual-end))))
    (+jg-twitter-tweet-with-input selection t)
    )
)

(defun +jg-twitter-tweet-with-input (input &optional reply)
  (if (string-equal (buffer-name) +jg-twitter-tweet_buff_name)
      (+jg-twitter-twitter-tweet)
    (progn
      (evil-window-new (get-buffer-window (current-buffer))
                       +jg-twitter-tweet_buff_name)
      (set (make-local-variable 'backup-inhibited) t)
      (if (and reply +jg-twitter-last-tweet-id)
          (set (make-local-variable 'reply_id) +jg-twitter-last-tweet-id)
          )
      (auto-save-mode -1)
      (evil-window-set-height 10)
      (with-current-buffer +jg-twitter-tweet_buff_name
        (+jg-twitter-tweet-minor-mode)
        (if input
            (insert input)
          )
        )
      (redraw-display)
      )
    )
  )

(defun +jg-twitter-twitter-tweet ()
  """ Actually tweet """
  (interactive)
  (+jg-twitter-twitter-tweet-text (buffer-string)
                                  (buffer-local-variables)
                                  '(+jg-twitter-tweet_sentinel))
  )

(defun +jg-twitter-twitter-tweet-text (text &optional locals sentinels)
  ;; Construct the twurl command:
  (let ((cmd (format "status=%s" text)))
    ;; If an image has been attached:
    (if (assq 'media_id locals)
        (setq cmd (format "%s&media_ids=%s" cmd
                          (cdr (assq 'media_id locals)))))

    ;; If the tweet is a reply to the last tweet:
    (if (assq 'reply_id locals)
        (setq cmd (format "%s&in_reply_to_status_id=%s" cmd
                          (cdr (assq 'reply_id locals)))))

    (if (get-buffer +jg-twitter-twurl_buff_name)
        (kill-buffer +jg-twitter-twurl_buff_name))

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
        (setq +jg-twitter-twitter-image-helm-source
              (helm-make-source "Find Image" 'helm-source-ffiles
                :action (helm-make-actions "action" '+jg-twitter-twitter-upload-image))
              ))
    (helm :sources +jg-twitter-twitter-image-helm-source
          :input "/Volumes/documents/DCIM/")
  )

(defun +jg-twitter-twitter-upload-image (candidate)
  " Initialise the upload "
  ;; TODO set media type better
  (let* ((sz (f-size candidate))
         (cmd (format "command=%s&total_bytes=%s&media_type=image/png" "INIT" sz))
         (target-buffer (buffer-name))
         )
    (message "Initialising Image Upload: -X POST -H %s -f %s %s"
             +jg-twitter-twurl_media_host
             candidate
             +jg-twitter-twurl_upload
             )
    (if (> sz 5242880) (message "File too large")
      (progn
        (setq +jg-twitter-twurl_file candidate
              +jg-twitter-twurl_target_tweet target-buffer)
        (with-current-buffer (get-buffer-create +jg-twitter-twurl_buff_name)
          (erase-buffer)
          (beginning-of-buffer))

        (start-process +jg-twitter-twurl_proc_name
                       +jg-twitter-twurl_buff_name
                       "twurl" "-X" "POST" "-H" +jg-twitter-twurl_media_host
                       "-f" candidate +jg-twitter-twurl_upload)
        (set-process-sentinel (get-process +jg-twitter-twurl_proc_name)
                              '+jg-twitter-add_media_id_to_tweet)
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
    (set-process-sentinel (get-process +jg-twitter-twurl_proc_name)
                          '+jg-twitter-add_media_id_to_tweet)
    )
  )

(defun +jg-twitter-add_media_id_to_tweet (&rest args)
  """ Actually insert the media_id to the local vars of the tweet buffer """
  (let* (obj errs err err-code err-msg)
    (with-current-buffer +jg-twitter-twurl_buff_name
      ;; Get the result
      (goto-char (point-min))
      (setq obj (json-parse-buffer))
      (if obj
          (erase-buffer))
      )

    (setq errs (cdr (assq 'errors obj))
          err (if errs (aref errs 0))
          err-code (if err (cdr (assq 'code err)))
          err-msg (if err (cdr (assq 'message err))))

    (if err
        (message (format "Image Upload Failed (%s): %s" err-code err-msg))
      (with-current-buffer +jg-twitter-twurl_target_tweet
        (make-local-variable 'media_id)
        (setq media_id +jg-twitter-twurl_media_id)
        (message "Media Inserted")
        ))
    )
  )

(defun +jg-twitter-tweet_sentinel (&rest args)
  """ Cleanup after tweeting """
  (set-buffer-modified-p nil)
  (with-current-buffer +jg-twitter-tweet_buff_name
    (setq +jg-twitter-last-tweet-text-backup
          (buffer-substring-no-properties (point-min)
                                          (point-max)))
    (kill-buffer-and-window)
    )
  (redraw-display)
  (with-current-buffer +jg-twitter-twurl_buff_name
    ;; Get the result
    (goto-char (point-min))
    (let* ((obj (json-parse-buffer))
           (errs (gethash "errors" obj nil))
           (err (if errs (aref errs 0)))
           (err-code (if err (gethash "code" err nil)))
           (err-msg (if err (gethash "message" err nil)))
           (id_str (gethash "id_str" obj nil))
           )
      (if err
          (message (format "Tweet Failed (%s): %s" err-code err-msg))
        (progn
          (message (format "Tweet Sent, id: %s" id_str))
          (setq +jg-twitter-last-tweet-id id_str)
          (kill-buffer +jg-twitter-twurl_buff_name)
          )
        )
      )
    )
  )
