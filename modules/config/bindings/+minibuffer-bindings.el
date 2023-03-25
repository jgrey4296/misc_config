;;; +minibuffer-bindings.el -*- lexical-binding: t; -*-

(setq minibuffer-mode-map (make-keymap))
(setq default-minibuffer-maps
      '(minibuffer-local-map
       minibuffer-local-ns-map
       minibuffer-local-completion-map
       minibuffer-local-must-match-map
       minibuffer-local-isearch-map
       read-expression-map
       ivy-minibuffer-map
       ivy-switch-buffer-map)
)

(map! :map (ivy-minibuffer-map minibuffer-mode-map)
      :n "<" #'beginning-of-line
      :n ">" #'end-of-line
      :i "j" #'self-insert-command
      :nv "j" #'ivy-next-line
      :n "v" #'ignore
      )

(map! :map minibuffer-mode-map
      :i "j" #'self-insert-command
      :i "|" #'minibuffer-keyboard-quit
      "RET"  #'exit-minibuffer
      )

(define-key! :keymaps +default-minibuffer-maps
  [escape] #'abort-recursive-edit
  "C-a"    #'move-beginning-of-line
  "C-r"    #'evil-paste-from-register
  "C-u"    #'evil-delete-back-to-indentation
  "C-v"    #'yank
  "C-w"    #'doom/delete-backward-word
  "C-z"    (cmd! (ignore-errors (call-interactively #'undo)))
  )

(define-key! :keymaps +default-minibuffer-maps
  "C-j"    #'next-line
  "C-k"    #'previous-line
  "C-S-j"  #'scroll-up-command
  "C-S-k"  #'scroll-down-command)

;; For folks with `evil-collection-setup-minibuffer' enabled
(define-key! :states 'insert :keymaps +default-minibuffer-maps
  "C-j"    #'next-line
  "C-k"    #'previous-line)

(map! :map (evil-ex-completion-map evil-ex-search-keymap)
      "C-a" #'evil-beginning-of-line
      "C-b" #'evil-backward-char
      "C-f" #'evil-forward-char
      :gi "C-j" #'next-complete-history-element
      :gi "C-k" #'previous-complete-history-element)

(define-key! :keymaps +default-minibuffer-maps
  [escape] #'abort-recursive-edit
  "C-a"    #'move-beginning-of-line
  "C-r"    #'evil-paste-from-register
  "C-u"    #'evil-delete-back-to-indentation
  "C-v"    #'yank
  "C-w"    #'doom/delete-backward-word
  "C-z"    (cmd! (ignore-errors (call-interactively #'undo))))

(define-key! :keymaps +default-minibuffer-maps
  "C-j"    #'next-line
  "C-k"    #'previous-line
  "C-S-j"  #'scroll-up-command
  "C-S-k"  #'scroll-down-command)
;; For folks with `evil-collection-setup-minibuffer' enabled
(define-key! :states 'insert :keymaps +default-minibuffer-maps
  "C-j"    #'next-line
  "C-k"    #'previous-line)
(define-key! read-expression-map
  "C-j" #'next-line-or-history-element
  "C-k" #'previous-line-or-history-element)

(map! :map ivy-minibuffer-map
      :after ivy
      [remap doom/delete-backward-word] #'ivy-backward-kill-word
      :ni "TAB"                         #'ivy-alt-done
      :i "<backtab>"                    #'ivy-dispatching-call
      :n  ","                           #'+ivy/woccur
      :n  "."                           #'hydra-ivy/body

      :n "o" #'hydra-ivy/body
      :n "a" #'ivy-dispatching-done

      :n "j" #'ivy-next-line
      :i "j" #'self-insert-command
      :n "k" #'ivy-previous-line
      :n "RET" #'ivy-done

      "C-c RET"                         #'+ivy/woccur
      "C-o"                             #'ivy-dispatching-done
      "C-h"                             #'ivy-backward-kill-word
      "M-o"                             #'hydra-ivy/body
      "C-M-o"                           #'ivy-dispatching-call
      "<down>"                          #'ivy-scroll-down-command
      "<up>"                            #'ivy-scroll-up-command

      :localleader
      :desc "Results as Buffer"        :n "b" #'+ivy/woccur
      )

(map! :map ivy-minibuffer-map
      :after ivy
      "C-SPC" #'ivy-call-and-recenter  ; preview file
      "C-l"   #'ivy-alt-done
      "C-v"   #'yank
      :n "|"  #'abort-recursive-edit
      :i "|" #'self-insert-command
      )
