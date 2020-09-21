;;; emacs/dired/+bindings.el -*- lexical-binding: t; -*-

;;;###package dired-git-info
(map! :after dired
      :map (dired-mode-map ranger-mode-map)
      :ng ")" #'dired-git-info-mode
      :n "o" #'dired-find-file-other-window
      :n "S" #'hydra-dired-quick-sort/body
      (:when (featurep! :main jg-personal)
      :n "i" #'+jg-personal-dired-insert-subdir-maybe-recursive
      :n "DEL" #'dired-kill-subdir
      (:localleader
       (:prefix ("d" . "Describe")
       "s" '+jg-personal-dired-create-summary-of-orgs
       "m" '+jg-personal-dired-marked-info
       "d" '+jg-personal-dired-diff
        )
       (:prefix ("K" . "Destructive")

        )
       (:prefix ("m" . "Mark")

        )
       (:prefix ("f" . "Find")

        )
       (:prefix ("g" . "gtags")

        )

       )
      )
)
