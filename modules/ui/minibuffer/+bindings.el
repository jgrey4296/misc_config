;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-normal-state-map
      :desc "From Minibuffer history"  "I m"          #'counsel-minibuffer-history
      )

;;-- ivy
(map! :map jg-minibuffer-ivy-map ;; general
      ;; :g [escape]   #'+jg-minibuffer-normal-or-exit
      :ni "TAB"        #'ivy-alt-done
      :ing  "RET"      #'ivy-done
      :n "DEL"         #'counsel-minibuffer-history
      :i  "<backtab>"  #'ivy-dispatching-call
      :n  ","          #'+ivy/woccur

      :n "/"           #'+ivy/woccur

      :n "m"           #'+jg-ivy-toggle-mark
      :n "t"           #'ivy-toggle-marks
      :n  "q"          #'abort-recursive-edit
      :n  "a"          #'ivy-dispatching-done
      :n  "o"          #'jg-ivy-hydra/body
      :n  "."          #'jg-ivy-hydra/body

      :n  "|"          #'abort-recursive-edit
      :i  "|"          #'self-insert-command

      :nv "j"          #'ivy-next-line
      :i  "j"          #'self-insert-command
      :n  "k"          #'ivy-previous-line

      :n  "K"          #'previous-history-element
      :n  "J"          #'next-history-element

      :n  "<"          #'beginning-of-line
      :n  ">"          #'end-of-line
      :n  "v"          #'ignore
)
(map! :map jg-minibuffer-ivy-map ;; C-{}
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
(map! :map jg-minibuffer-ivy-map ;; local
      :localleader
      :desc "Results as Buffer"        :n "b" #'+ivy/woccur
      )

;;-- end ivy

;;-- minibuffer-completion
(map! :map jg-minibuffer-local-map
      [escape]       #'abort-recursive-edit
      :ni "RET"       #'exit-minibuffer
      :n "DEL"       #'counsel-minibuffer-history
      :n "j" nil
      :n "k" nil
      ;; :n "j"      #'next-line-or-history-element
      ;; :n "k"      #'previous-line-or-history-element
      ;; :n "j"         #'next-line
      ;; :n "k"         #'previous-line
      :n "|"         #'minibuffer-keyboard-quit
      :i "j"         #'self-insert-command
      :i "C-j"       #'next-line-or-history-element
      :i "C-k"       #'previous-line-or-history-element
      )

(map! :map (jg-minibuffer-evil-ex-completion-map jg-minibuffer-evil-ex-search-keymap)
      :ni "RET" #'exit-minibuffer
      :n  "DEL" #'counsel-minibuffer-history
      :gi "C-j" #'next-complete-history-element
      :gi "C-k" #'previous-complete-history-element
      :n "k"    #'previous-line-or-history-element
      :n "j"    #'next-line-or-history-element

      "C-a"     #'evil-beginning-of-line
      "C-b"     #'evil-backward-char
      "C-f"     #'evil-forward-char
      )

(map! :map jg-minibuffer-read-expression-map
      :n "DEL"  #'counsel-minibuffer-history
      :i "DEL"  #'backward-delete-char
      :ni "RET"  #'read--expression-try-read
      "C-j"     #'next-line-or-history-element
      "C-k"     #'previous-line-or-history-element
      :n "k"    #'previous-history-element
      :n "j"    #'next-history-element
      :n "q"    #'minibuffer-keyboard-quit
      )

;;-- end minibuffer-completion

(after! ivy
  (setq ivy-minibuffer-map jg-minibuffer-ivy-map)
  )

(setq-default minibuffer-local-map   jg-minibuffer-local-map)
(setq evil-ex-completion-map jg-minibuffer-evil-ex-completion-map
      evil-ex-search-keymap  jg-minibuffer-evil-ex-search-keymap
      read-expression-map    jg-minibuffer-read-expression-map
      read--expression-map   jg-minibuffer-read-expression-map
      )
