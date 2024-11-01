;;; +bindings.el -*- lexical-binding: t; -*-

(evil-make-intercept-map jg-term-shell-mode-map)
(evil-make-intercept-map jg-term-comint-mode-map)

(map! :leader
      :desc "Pop Shell"             "'"   #'+jg-shell-new
      )

(map! :map jg-term-shell-mode-map
      :n "C-d" #'comint-send-eof
      :n "DEL" #'counsel-shell-history
      :ni "RET" #'comint-send-input
      :i "TAB" #'completion-at-point

      :n "," #'+jg-term-switch
      :n "b" #'shell-backward-command
      :n "w" #'shell-forward-command
      :n "H" #'comint-show-output
      :n "L" #'comint-show-maximum-output

      :localleader
      "h"   #'counsel-shell-history
      "q"   #'comint-quit-subjob
      "i"   #'comint-interrupt-subjob
      "z"   #'comint-stop-subjob
      "w"   #'comint-write-output
      "e"   #'comint-send-eof

      "TAB" #'toggle-truncate-lines
      )

;; overrides the default normal mode binding of evil-ret
(map! :map jg-term-comint-mode-map
      :n "C-d" #'comint-send-eof

      :n "H" #'comint-show-output
      :n "L" #'comint-show-maximum-output
      :ni "RET" #'comint-send-input

      :localleader
      "q"   #'comint-quit-subjob
      "i"   #'comint-interrupt-subjob
      "z"   #'comint-stop-subjob
      "w"   #'comint-write-output
      "e"   #'comint-send-eof
      )

(after! (shell comint)
  (setq shell-mode-map jg-term-shell-mode-map
        comint-mode-map jg-term-comint-mode-map
        )
  )
