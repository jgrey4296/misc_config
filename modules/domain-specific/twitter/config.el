 (setq-default +jg-twitter-twurl_buff_name "*twurl*"
               +jg-twitter-twurl_proc_name "*twurl-proc*"
               +jg-twitter-twurl_command_name "twurl"
               +jg-twitter-twurl_media_host "upload.twitter.com"
               +jg-twitter-twurl_default_host "api.twitter.com"
               +jg-twitter-twurl_upload "/1.1/media/upload.json"
               +jg-twitter-twurl_tweet "/1.1/statuses/update.json"
               +jg-twitter-twurl_file nil
               +jg-twitter-twurl_media_id nil
               +jg-twitter-twurl_target_tweet nil
               +jg-twitter-tweet_buff_name "*tweet*"
)

(after! helm
  (setq-default +jg-twitter-twitter-image-helm-source
                (helm-make-source "Find Image" 'helm-source-ffiles
                  :action '(("action" . +jg-twitter-twitter-upload-image))))
    )

(after! evil
  (evil-ex-define-cmd "tweet" '+jg-twitter-tweet)
  (evil-ex-define-cmd "image" '+jg-twitter-twitter-add-picture)
  ;; TODO (spacemacs/set-leader-keys "a U t" '+jg-twitter-open_user)

  )

(load! "+funcs")
