;;; domain-specific/twitter/+tweet-minor-mode.el -*- lexical-binding: t; -*-

(defvar jg-twitter-max-tweet-length 250)
(defvar jg-twitter-oversize-face `((background-color . "red")))

(defun +jg-twitter-tweet-length-check (beg end len)
  "After change function to ensure the tweet length is allowed"
  (if (> (point-max) jg-twitter-max-tweet-length)
      ;; To Big:
      (move-overlay jg-twitter-overlay
                    jg-twitter-max-tweet-length
                    (point-max))
    ;; Allowed:
    (delete-overlay jg-twitter-overlay)
    )

)

(define-minor-mode +jg-twitter-tweet-minor-mode
  "A Minor mode to check tweet lengths"
  :lighter "TWEET"
  (if (not +jg-twitter-tweet-minor-mode)
      (progn ;; Disabling
        (message "Disabling Tweet Overlay")
        (if (boundp 'jg-twitter-overlay)
            (delete-overlay jg-twitter-overlay))
        )
    (progn ;; Enabling
      ;; Register buffer local change-hook
      (message "Enabling tweet overlay")
      (make-local-variable 'after-change-functions)
      (make-local-variable 'jg-twitter-overlay)
      (push #'+jg-twitter-tweet-length-check after-change-functions)
      (setq-local jg-twitter-overlay (make-overlay (point-max) (point-max)))
      (overlay-put jg-twitter-overlay
                   'face jg-twitter-oversize-face)
      )
    )
  )
