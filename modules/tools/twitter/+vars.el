;;; domain-specific/twitter/+vars.el -*- lexical-binding: t; -*-

(setq-default +jg-twitter-tweet_buff_name "*tweet*"
              +jg-twitter-twurl_buff_name "*twurl*"
              +jg-twitter-twurl_proc_name "*twurl-proc*"
              +jg-twitter-twurl_command_name "twurl"
              +jg-twitter-twurl_media_host "upload.twitter.com"
              +jg-twitter-twurl_default_host "api.twitter.com"
              +jg-twitter-twurl_upload "/1.1/media/upload.json"
              +jg-twitter-twurl_tweet "/1.1/statuses/update.json"
              +jg-twitter-twurl_file nil
              +jg-twitter-twurl_media_id nil
              +jg-twitter-twurl_target_tweet nil
              +jg-twitter-last-tweet-text-backup nil
              +jg-twitter-last-tweet-id nil
)

(setq-default jg-twitter-download-repo "/Volumes/documents/github/py_bookmark_organiser/"
              jg-twitter-download-py   "bkmkorg/io/twitter/automator.py"
              )

;;-- lookup spec
(spec-handling-add! lookup-url
                    '(twitter
                     ("Twitter"            "https://twitter.com/%s")
                     )
                    )

;;-- end lookup spec
