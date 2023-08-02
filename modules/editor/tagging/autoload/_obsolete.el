;;; _obsolete.el -*- lexical-binding: t; -*-


(defun +jg-tag-tweet-link-action (candidate)
  "Helm action to open a tweet buffer with the link inserted"
  (evil-window-new (get-buffer-window helm-current-buffer)
                   "*Link Tweeting*")
  (set (make-local-variable 'backup-inhibited) t)
  (auto-save-mode -1)
  (evil-window-set-height 10)
  (evil-initialize-local-keymaps)
  (evil-local-set-key 'normal
                      (kbd "C-c C-C") '+jg-tag-tweet-link-finish)
  (insert "\n")
  (insert (plist-get candidate :url))
  (redraw-display)
  )

(defun +jg-tag-tweet-link-finish ()
  "Action to finish and tweet a link"
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max))))
    (+jg-twitter-twitter-tweet-text text nil '(+jg-twitter-tweet_sentinel))
    ))
