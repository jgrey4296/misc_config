;;; +bindings.el -*- lexical-binding: t; -*-

(evil-make-intercept-map jg-term-shell-mode-map)
(evil-make-intercept-map jg-term-comint-mode-map)
(with-state! 'normal #'counsel-shell-history)
(with-state! 'normal #'+jg-term-switch)

(map! :leader
      :desc "Pop Shell"             "'"   #'+jg-shell-new
      )

(map! :map jg-term-shell-mode-map
      :n "\\"   #'+jg-term-column-motion
      :n "C-d"  #'comint-send-eof
      :n "DEL"  #'counsel-shell-history--with-state-normal
      :ni "RET" #'comint-send-input
      :i "TAB"  #'completion-at-point

      :n ","    #'+jg-term-switch--with-state-normal
      :n "H"    #'comint-show-output
      :n "L"    #'comint-show-maximum-output

      )

(map! :map jg-term-shell-mode-map
      :localleader
      "h"   #'counsel-shell-history--with-state-normal
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
      :n "[ p" #'comint-previous-prompt
      :n "] p" #'comint-next-prompt
      )

(map! :map jg-term-comint-mode-map
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
