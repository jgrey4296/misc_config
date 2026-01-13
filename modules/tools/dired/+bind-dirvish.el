;;; +bind-dirvish.el -*- lexical-binding: t; no-byte-compile: t; -*-


(map! :map dirvish-mode-map
      :n "b" #'dirvish-goto-bookmark
      :n "z" #'dirvish-show-history
      :n "f" #'dirvish-file-info-menu
      :n "F" #'dirvish-toggle-fullscreen
      :n "l" #'dired-find-file
      :n "h" #'dired-up-directory
      :localleader
      "h" #'dired-omit-mode
      )


;;; +bind-dirvish.el ends here
