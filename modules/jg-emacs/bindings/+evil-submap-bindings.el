;;; config/default/+bindings.el -*- lexical-binding: t; -*-

(message "Setting up Evil Submaps")
(map! :after evil-ex
      :map (evil-ex-completion-map evil-ex-search-keymap)
      "C-a" #'evil-beginning-of-line
      "C-b" #'evil-backward-char
      "C-f" #'evil-forward-char
      :gi "C-j" #'next-complete-history-element
      :gi "C-k" #'previous-complete-history-element)

(map! :after jg-evil-bindings
      :map jg-binding-insert-state-map
      "C-j"    #'next-line
      "C-k"    #'previous-line
      )

;; For folks with `evil-collection-setup-minibuffer' enabled
(define-key! :states 'insert :keymaps +default-minibuffer-maps
  "C-j"    #'next-line
  "C-k"    #'previous-line)

(define-key! read-expression-map
  "C-j" #'next-line-or-history-element
  "C-k" #'previous-line-or-history-element)
