;;; +bindings.el -*- lexical-binding: t; -*-

(defvar jg-minibuffer-maps
  '(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    evil-ex-completion-map
    read-expression-map)
  "A list of all the keymaps used for the minibuffer."
  )

(defvar jg-minibuffer-ivy-map (make-sparse-keymap))

(defvar jg-minibuffer-local-map (make-sparse-keymap))

(defvar jg-minibuffer-read-expression-map (make-sparse-keymap))

(defvar jg-minibuffer-evil-ex-completion-map (make-sparse-keymap))

(defvar jg-minibuffer-evil-ex-search-keymap (make-sparse-keymap))

(map! :map jg-minibuffer-ivy-map
      ;; :g [escape]  #'+jg-minibuffer-normal-or-exit
      :ni "TAB"       #'ivy-alt-done
      :i  "<backtab>" #'ivy-dispatching-call
      :n  ","         #'+ivy/woccur

      :n "/"          #'+ivy/woccur

      :n "m"          #'+jg-ivy-toggle-mark
      :n "t"          #'ivy-toggle-marks
      :n  "q"         #'abort-recursive-edit
      :n  "a"         #'ivy-dispatching-done
      :n  "o"         #'jg-ivy-hydra/body
      :n  "."         #'jg-ivy-hydra/body
      :ing  "RET"      #'ivy-done

      :n  "|"         #'abort-recursive-edit
      :i  "|"         #'self-insert-command

      :nv "j"         #'ivy-next-line
      :i  "j"         #'self-insert-command
      :n  "k"         #'ivy-previous-line

      :n  "K"         #'previous-history-element
      :n  "J"         #'next-history-element

      :n  "<"         #'beginning-of-line
      :n  ">"         #'end-of-line
      :n  "v"         #'ignore
)
(map! :map jg-minibuffer-ivy-map
      "C-g"     #'+jg-minibuffer-normal-or-exit
      "C-SPC"   #'ivy-call-and-recenter  ;; preview file
      "C-l"     #'ivy-alt-done
      "C-v"     #'yank
      "C-c RET" #'+ivy/woccur
      "C-o"     #'ivy-dispatching-done
      "C-h"     #'ivy-backward-kill-word
      "M-o"     #'hydra-ivy/body
      "C-M-o"   #'ivy-dispatching-call
      "<down>"  #'ivy-scroll-down-command
      "<up>"    #'ivy-scroll-up-command
      )
(map! :map jg-minibuffer-ivy-map
      :localleader
      :desc "Results as Buffer"        :n "b" #'+ivy/woccur
      )

(map! :map jg-minibuffer-local-map
      [escape] #'abort-recursive-edit
      :g "RET" #'exit-minibuffer
      :ni "RET" #'exit-minibuffer
      :i "j"   #'self-insert-command
      :n "|"   #'minibuffer-keyboard-quit
      :g "RET" #'exit-minibuffer
      )
(map! :map jg-minibuffer-local-map
      "C-j"    #'next-line
      "C-k"    #'previous-line
      "C-S-j"  #'scroll-up-command
      "C-S-k"  #'scroll-down-command
      :i "C-j"    #'next-line
      :i "C-k"    #'previous-line
      )

(map! :map (jg-minibuffer-evil-ex-completion-map jg-minibuffer-evil-ex-search-keymap)
      :g "RET" #'exit-minibuffer
      "C-a" #'evil-beginning-of-line
      "C-b" #'evil-backward-char
      "C-f" #'evil-forward-char
      :gi "C-j" #'next-complete-history-element
      :gi "C-k" #'previous-complete-history-element
      :n "k" #'previous-history-element
      :n "j" #'next-history-element
      )

(map! :map jg-read-expression-map
  "C-j" #'next-line-or-history-element
  "C-k" #'previous-line-or-history-element
  :n "k" #'previous-history-element
  :n "j" #'next-history-element
  )

(map! :map ctl-x-map
      "[" "("
      "]" ")"
      )

(after! ivy
  (setq ivy-minibuffer-map jg-minibuffer-ivy-map
        )
  )

(setq minibuffer-local-map jg-minibuffer-local-map
      evil-ex-completion-map jg-minibuffer-evil-ex-completion-map
      evil-ex-search-keymap jg-minibuffer-evil-ex-search-keymap
      )
