;;; org/jg-org/+org-pomodoro-funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-org-pomodoro-start-hook ()
  ;; tweet out start and end points
  ;; use org-pomodoro-end-time
  (+jg-twitter-twitter-tweet-text
   (format "Emacs Pomodoro Timer Session to end: %s"
           (format-time-string "%H:%M (%d, %b, %Y)" org-pomodoro-end-time)))
  )

;;;###autoload
(defun +jg-org-pomodoro-end-hook ()
  ;; create the temp buffer
  (progn
    (evil-window-new (get-buffer-window (current-buffer))
                     +jg-org-pomodoro-buffer-name)
    (set (make-local-variable 'backup-inhibited) t)
    (auto-save-mode -1)
    (evil-window-set-height 10)
    (evil-initialize-local-keymaps)
    (evil-local-set-key 'normal (kbd "C-c C-c")
                        '+jg-org-pomodoro-finish)
    (insert +jg-org-pomodoro-log-message)
    (insert "Pomodoro Session: ")
    (redraw-display)
    )
  )

;;;###autoload
(defun +jg-org-pomodoro-finish ()
  ;; get the text
  (interactive)
  (let* ((text (buffer-substring (length +jg-org-pomodoro-log-message) (point-max)))
         (time (format-time-string "(%Y/%b/%d) %H:%M" (current-time)))
         (formatted (format "** %s\n    %s\n" time (string-trim text)))
         )
    ;; tweet it
    (+jg-twitter-twitter-tweet-text text nil '(+jg-twitter-tweet_sentinel))
    ;;add it to the pomodoro log file
    (append-to-file formatted nil (expand-file-name +jg-org-pomodoro-log-file))
    )
  )
