;;; completion/ivy/+bindings.el -*- lexical-binding: t; -*-
(message "Loading completion/jg-ivy/+bindings.el")

(define-key! ivy-minibuffer-map
  [remap doom/delete-backward-word] #'ivy-backward-kill-word
  "C-c C-e"                         #'+ivy/woccur
  "C-o"                             #'ivy-dispatching-done
  "M-o"                             #'hydra-ivy/body
  )

(map! :map evil-normal-state-map
      :prefix "g"
      "g" #'company-manual-begin)
