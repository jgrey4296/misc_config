;;; emacs/dired/+bindings.el -*- lexical-binding: t; -*-


(defun +jg-dired-binding-hook ()
  (message "Setting up jg-dired: %s" (current-time-string))
  (map! :map (dired-mode-map ranger-mode-map)
       :n ")"                                #'dired-git-info-mode
       :n "o"                                #'dired-find-file-other-window
       :n "S"                                #'hydra-dired-quick-sort/body
       :n "j" #'dired-next-line
       :n "k" #'dired-previous-line
       :n "J" #'dired-next-dirline
       :n "K" #'dired-prev-dirline

       :n "v" nil
       :n "i" nil
       ":" nil

      (:prefix "]"
       :desc "Next Marked" :n "m" #'dired-next-marked-file
       )
      (:prefix "["
       :desc "Prev Marked" :n "m" #'dired-prev-marked-file
       )
      :desc "Expand Subdir" :n "i"                              #'+jg-dired-insert-subdir-maybe-recursive
      :desc "Remove Subdir" :n "DEL"                            #'dired-kill-subdir
      (:when (featurep! :lang python)
       :n "v" 'pyvenv-activate
       )
      (:prefix "%"
      :desc "Global Match Rename" :n "R" #'+jg-GLOBAL-dired-do-rename-regexp)
      )

  (assert (lookup-key dired-mode-map [normal-state ?i]) #'+jg-dired-insert-subdir-maybe-recursive)

  (map! :map dired-mode-map
        :localleader
        :desc "Hide Toggle" "h" #'dired-omit-mode
        (:prefix ("d" . "Describe")
         :desc "Summarise Orgs" "s"         #'+jg-dired-create-summary-of-orgs
         :desc "Marked Info" "m"            #'+jg-dired-marked-info
         :desc "Dired Diff" "d"             #'+jg-dired-diff
         )
        (:prefix ("K" . "Destructive")
         :desc "Reformat jsons"   "J" #'+jg-dired-reformat-jsons
         )
        (:prefix ("f" . "Find")
         :desc "Find Random Marked" "r" #'+jg-dired-find-random-marked-file
         )
      )

  (evil-make-overriding-map dired-mode-map)

  )
