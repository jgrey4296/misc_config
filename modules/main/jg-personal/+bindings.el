
(map! :leader
      (:prefix "t"
      :n "h" #'global-auto-highlight-symbol-mode
      :n "c" #'centered-cursor-mode
      :n "v" #'evil-visual-mark-mode)
      (:prefix "x"
      :n "e" #'evil-iedit-state/iedit-mode)
      :n "f t" 'neotree-toggle
      )

(map! :after python-mode
      :mode python-mode
      :prefix "SPC i"
      "d" '+jg-personal-python-toggle-breakpoint
      )
(map! :after (python-mode helm helm-gtags)
      :mode 'python-mode
      :prefix "SPC j"
      "C" 'helm-gtags-create-tags
      "d" 'helm-gtags-find-tag
      "D" 'helm-gtags-find-tag-other-window
      "G" 'helm-gtags-dwim-other-window
      "i" 'helm-gtags-tags-in-this-function
      "l" 'helm-gtags-parse-file
      "n" 'helm-gtags-next-history
      "p" 'helm-gtags-previous-history
      "r" 'helm-gtags-find-rtag
      "R" 'helm-gtags-resume
      "s" 'helm-gtags-select
      "S" 'helm-gtags-show-stack
      "y" 'helm-gtags-find-symbol
      "U" 'helm-gtags-update-tags)
(map! :mode vlf-mode
      :n "] A" 'vlf-next-batch-from-point
      :n "] a" 'vlf-next-batch
      :n "[ a" 'vlf-prev-batch
      :n "SPC a U v " 'vlf-set-batch-size
      )

(map! :after python-mode
      :map 'python-mode-map
      :n "z d" '+jg-personal-toggle-all-defs
      :n "z C" '+jg-personal-close-class-defs
      )
(map! :after (dired python-mode)
      :map dired-mode-map
      :localleader
      :n "v" 'pyvenv-activate
      )
(map! :after (evil hi-lock)
      :map evil-normal-state-map
      :prefix "z '"
      "i" 'hi-lock-write-interactive-patterns
      "u" 'unhighlight-regexp
      "." 'highlight-symbol-at-point
      "r" 'highlight-regexp
      "p" 'highlight-phrase
      "l" 'highlight-lines-matching-regexp
      "f" 'hi-lock-find-patterns
      )
