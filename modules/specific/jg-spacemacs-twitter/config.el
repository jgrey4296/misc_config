 (setq-default jg-spacemacs-twitter/twurl_buff_name "*twurl*"
               jg-spacemacs-twitter/twurl_proc_name "*twurl-proc*"
               jg-spacemacs-twitter/twurl_command_name "twurl"
               jg-spacemacs-twitter/twurl_media_host "upload.twitter.com"
               jg-spacemacs-twitter/twurl_default_host "api.twitter.com"
               jg-spacemacs-twitter/twurl_upload "/1.1/media/upload.json"
               jg-spacemacs-twitter/twurl_tweet "/1.1/statuses/update.json"
               jg-spacemacs-twitter/twurl_file nil
               jg-spacemacs-twitter/twurl_media_id nil
               jg-spacemacs-twitter/twurl_target_tweet nil
               jg-spacemacs-twitter/tweet_buff_name "*tweet*"
)

(defun jg-spacemacs-twitter/pre-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config
    (setq-default jg-spacemacs-twitter/twitter-image-helm-source
                  (helm-make-source "Find Image" 'helm-source-ffiles
                    :action '(("action" . jg-spacemacs-twitter/twitter-upload-image))))
    )
  )

(defun jg-spacemacs-twitter/init-tramp ()
  (use-package tramp
    :defer t)
  )

(defun jg-spacemacs-twitter/post-init-evil ()
  (evil-ex-define-cmd "tweet" 'jg-spacemacs-twitter/tweet)
  (evil-ex-define-cmd "image" 'jg-spacemacs-twitter/twitter-add-picture)
  (spacemacs/set-leader-keys "a U t" 'jg-spacemacs-twitter/open_user)

  )
