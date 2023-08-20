;;; +funcs.el -*- lexical-binding: t; -*-
(require 's)

(defvar fd-dired-minor-mode-map (make-sparse-keymap))
(evil-define-key 'normal fd-dired-minor-mode-map (kbd "<RET>") #'dired-find-file-other-window)
(evil-define-key 'normal fd-dired-minor-mode-map "<" (cmd! (find-file "~")))
(evil-make-overriding-map fd-dired-minor-mode-map)

(define-minor-mode fd-dired-minor-mode
  " A Minor Mode for FD-Dired results "
  :lighter "Fd-Dired"
  :global nil
  :interactive t
  :keymap fd-dired-minor-mode-map
  (if (s-equals? (buffer-name) "*Fd*")
      (message "Fd minor mode enabled")
    (setq fd-dired-minor-mode nil)
    )
  )

;; (fd-dired-minor-mode 0)
