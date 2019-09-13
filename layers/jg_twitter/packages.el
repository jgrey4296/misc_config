(defconst jg_twitter-packages
  '(
    tramp
    evil
    helm
    )
  )

(defun jg_twitter/pre-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config
    (setq-default jg_twitter/twitter-image-helm-source
                  (helm-make-source "Find Image" 'helm-source-ffiles
                    :action '(("action" . jg_twitter/twitter-upload-image))))
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

