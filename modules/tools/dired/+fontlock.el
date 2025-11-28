;;; +fontlock.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package! diredfl ;; font lock
  :hook (dired-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-flag-mark-line nil :background "blueviolet")
  )

;;; +fontlock.el ends here
