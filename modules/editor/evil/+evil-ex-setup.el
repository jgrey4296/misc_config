;;; main/personal/+evil-setup.el -*- lexical-binding: t; -*-

(doom-log "Setting up Evil-Ex bindings: %s" (current-time-string))

(setq evil-ex-commands nil )

(map! :map (evil-ex-completion-map evil-ex-search-keymap)
      "C-a"                             #'evil-beginning-of-line
      "C-b"                             #'evil-backward-char
      "C-f"                             #'evil-forward-char
      :gi "C-j"                         #'next-complete-history-element
      :gi "C-k"                         #'previous-complete-history-element)

;; definition of said commands, adapted from evil-maps

;;goto top of buffer
(evil-ex-define-cmd "top"                    #'evil-goto-char)

;;-- buffer / file loading
(evil-ex-define-cmd "w[rite]"           #'evil-write)
(evil-ex-define-cmd "wa[ll]"            #'evil-write-all)

(evil-ex-define-cmd "pwd"               #'+evil:pwd)

;;-- line ops
;; bring a line up
(evil-ex-define-cmd "="                 #'evil-ex-line-number)

;;-- evil states
(evil-ex-define-cmd "g[lobal]"          #'evil-ex-global)
(evil-ex-define-cmd "v[global]"         #'evil-ex-global-inverted)
(evil-ex-define-cmd "norm[al]"          #'evil-ex-normal)
(evil-ex-define-cmd "set-initial-state" #'evil-ex-set-initial-state)

;;-- end evil states

;;-- registers
(evil-ex-define-cmd "registers"         #'evil-show-registers)
(evil-ex-define-cmd "marks"             #'evil-show-marks)
(evil-ex-define-cmd "delm[arks]"        #'evil-delete-marks)
(evil-ex-define-cmd "ju[mps]"           #'evil-show-jumps)
(evil-ex-define-cmd "noh[lsearch]"      #'evil-ex-nohighlight)
(evil-ex-define-cmd "f[ile]"            #'evil-show-file-info)

;;-- end registers

;;-- shell
(evil-ex-define-cmd "!"                 #'evil-shell-command)
(evil-ex-define-cmd "@:"                #'evil-ex-repeat)
;;-- end shell

(evil-define-command +evil:swiper (&optional search)
  "Invoke `swiper' with SEARCH, otherwise with the symbol at point."
  (interactive "<a>")
  (swiper-isearch search))
(evil-ex-define-cmd "sw[iper]"          #'+evil:swiper)

(provide 'jg-evil-ex-bindings)
