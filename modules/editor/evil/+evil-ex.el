;;; main/personal/+evil-setup.el -*- lexical-binding: t; -*-

(dlog! "setting up evil-ex bindings: %s" (current-time-string))

;; These arg types will highlight matches in the current buffer
(evil-ex-define-argument-type regexp-match
  :runner (lambda (flag &optional arg) (+evil-ex-regexp-match flag arg 'inverted)))

(evil-ex-define-argument-type regexp-global-match
  :runner +evil-ex-regexp-match)

;; Other commands can make use of this
(evil-define-interactive-code "<//>"
  :ex-arg regexp-match
  (+evil--regexp-match-args evil-ex-argument))

(evil-define-interactive-code "<//!>"
  :ex-arg regexp-global-match
  (+evil--regexp-match-args evil-ex-argument))

(setq evil-ex-commands nil )

;; --------------------------------------------------

(map! :map (evil-ex-completion-map evil-ex-search-keymap)
      "C-a"                                  #'evil-beginning-of-line
      "C-b"                                  #'evil-backward-char
      "C-f"                                  #'evil-forward-char
      :gi "C-j"                              #'next-complete-history-element
      :gi "C-k"                              #'previous-complete-history-element
      )

;; --------------------------------------------------

;; evil ex
(setq evil-ex-search-vim-style-regexp t
      evil-ex-visual-char-range t                           ;; column range for ex commands
      evil-ex-interactive-search-highlight 'selected-window ;; Only do highlighting in selected window so that Emacs has less work to do highlighting them all.
      )

(speckler-new! evil-ex (key val)
  "Register and re-apply evil-ex cmds"
  :struct '(cmdstr . cmd)
  :loop 'do
  (cl-loop for x in val
           do
           (evil-ex-define-cmd (car x) (upfun! (cdr x)))
           )
  )
;; definition of said commands, adapted from evil-maps
(speckler-add! evil-ex ()
  '(default
    ("top"      . #'evil-goto-char)
    ("w[rite]"  . #'evil-write)
    ("wa[ll]"   . #'evil-write-all)
    ("W"        . #'evil-write-all)
    ("pwd"      . #'+evil:pwd)
    ("="        . #'evil-ex-line-number)
    ("list"     . #'+jg-evil-list-ex-commands)
    ("sw[iper]" . #'+evil:swiper)
    )
  '(states
    ("g[lobal]"               . #'evil-ex-global)
    ("v[global]"              . #'evil-ex-global-inverted)
    ("norm[al]"               . #'evil-ex-normal)
    ("set-initial-state"      . #'evil-ex-set-initial-state)
    )
  '(registers
    ("registers"              . #'evil-show-registers)
    ("marks"                  . #'evil-show-marks)
    ("delm[arks]"             . #'evil-delete-marks)
    ("ju[mps]"                . #'evil-show-jumps)
    ("noh[lsearch]"           . #'evil-ex-nohighlight)
    ("f[ile]"                 . #'evil-show-file-info)
    )
  '(shell
    ("!"  . #'evil-shell-command)
    ("@:" . #'evil-ex-repeat)
    )
  )

(provide 'jg-evil-ex-bindings)
