;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-normal-state-map
      :after undo-fu
      :desc "undo" "u" 'undo-fu-only-undo
      :desc "redo" "U" 'undo-fu-only-redo
      )

(map! :map jg-binding-operator-map
      :desc "Vundo"                       "u" #'vundo
      )


(map! :map vundo-mode-map
      :n "RET" #'vundo-confirm
      )
(map! :map vundo-mode-map
    [remap evil-backward-char] 'vundo-backward
    [remap evil-forward-char]  'vundo-forward
    [remap evil-next-line]     'vundo-next
    [remap evil-previous-line] 'vundo-previous
    [remap evil-window-top]    'vundo-stem-root
    [remap evil-window-bottom] 'vundo-stem-end
    [remap evil-ret]           'vundo-confirm
    [remap evil-quit]          'vundo-quit
    [remap evil-save-and-close] 'vundo-confirm
    [remap evil-save-modified-and-close] 'vundo-confirm
    [remap evil-delete-backward-char] 'vundo-backward
    :n "i" 'vundo--inspect
    :n "D" 'vundo--debug
    :n "d" 'vundo-diff
    :n "m" 'vundo-diff-mark
    :n "u" 'vundo-diff-unmark
    :n "w" 'vundo-next-root
    :n "q" 'vundo-quit
    :n "Q" 'vundo-quit
    )
