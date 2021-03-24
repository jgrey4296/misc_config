;;; lang/jg-org/+vars.el -*- lexical-binding: t; -*-

(setq-default +jg-external-file-link-types '("jpg" "jpeg" "png" "mp4" "html"))
;; set pomodoro log variable
(defcustom +jg-org-pomodoro-log-file "~/.doom.d/setup_files/pomodoro_log.org" "The Location of the Pomodoro Log File")
(defcustom +jg-org-pomodoro-buffer-name "*Pomodoro Log*"
  "The name of the Pomodoro Log Buffer to record what I did in")
(defcustom +jg-org-pomodoro-log-message ";; What did the last Pomodoro session accomplish? C-c to finish\n"
  "The message to add to the log buffer to spur comments")
