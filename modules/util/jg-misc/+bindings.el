;;; util/jg-misc/+bindings.el -*- lexical-binding: t; -*-
(evil-make-intercept-map messages-buffer-mode-map)

(map! :leader
      :desc "Have you Played?" "o h h" #'+jg-misc-helm-rps-have-you-playeds
      )

(map! :map help-map
      "DEL" #'free-keys
      )

(map! :map (sh-mode-map shell-mode-map)
      :localleader
      :desc "Docs: Brew"  "1" (cmd! (+jg-browse-url "https://brew.sh/"))
      :desc "Docs: Awk"   "2" (cmd! (+jg-browse-url "https://www.gnu.org/software/gawk/manual/gawk.html"))
      )

(map! :mode vlf-mode
      "] A" 'vlf-next-batch-from-point
      "] a" 'vlf-next-batch
      "[ a" 'vlf-prev-batch
      "SPC a U v " 'vlf-set-batch-size
      )

(map! :leader
      :prefix "t"
      :desc "Semantic" "S" #'semantic-mode
      )

(defun +jg-misc-free-key-binding-update ()
  (map! :map free-keys-mode-map
        :desc "Change Buffer" :n "b" #'free-keys-change-buffer
        :desc "Revert Buffer" :n "g" #'revert-buffer
        :desc "Describe Mode" :n "h" #'describe-mode
        :desc "Set Prefix"    :n "p" #'free-keys-set-prefix
        :desc "Quit"          :n "q" #'quit-window
        )
  (evil-make-intercept-map free-keys-mode-map)
  )
