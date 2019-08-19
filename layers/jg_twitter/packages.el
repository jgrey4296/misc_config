(defconst jg_twitter-packages
  '(
    tramp
    evil
    )
  )

(defun jg_twitter/init-tramp ()
  (use-package tramp
    :defer t)
  )

(defun jg_twitter/post-init-evil ()
  (evil-ex-define-cmd "tweet" 'jg_twitter/tweet)
  (evil-ex-define-cmd "image" 'jg_twitter/twitter-add-picture)
  )

