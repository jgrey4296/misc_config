;;; +funcs.el -*- lexical-binding: t; -*-
(require 's)

(defvar fd-dired-minor-mode-map (make-sparse-keymap))
;; (evil-make-overriding-map fd-dired-minor-mode-map)
(define-minor-mode fd-dired-minor-mode
  " A Minor Mode for FD-Dired results "
  :lighter "Fd-Dired"
  :global nil
  :interactive nil
  :keymap fd-dired-minor-mode-map
  (setq fd-dired-minor-mode (s-equals? (buffer-name) "*Fd*"))
  )

(map! :map fd-dired-minor-mode-map
      :n "RET" #'dired-find-file-other-window
      )
(fd-dired-minor-mode 0)
