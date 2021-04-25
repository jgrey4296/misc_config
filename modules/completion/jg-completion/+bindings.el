;;; completion/ivy/+bindings.el -*- lexical-binding: t; -*-

(defun +jg-completion-binding-hook ()
  (message "Setting up Completion bindings")

  (define-key! ivy-minibuffer-map
    [remap doom/delete-backward-word] #'ivy-backward-kill-word
    "C-c C-e"                         #'+ivy/woccur
    "C-o"                             #'ivy-dispatching-done
    "M-o"                             #'hydra-ivy/body
    )

)
