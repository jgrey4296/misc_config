
(map! :leader
      (:prefix "t"
      :n "h" #'global-auto-highlight-symbol-mode
      :n "c" #'centered-cursor-mode
      :n "v" #'evil-visual-mark-mode)
      (:prefix "x"
      :n "e" #'evil-iedit-state/iedit-mode)
      :n "f t" 'neotree-toggle
      )
(map! :mode vlf-mode
      :n "] A" 'vlf-next-batch-from-point
      :n "] a" 'vlf-next-batch
      :n "[ a" 'vlf-prev-batch
      :n "SPC a U v " 'vlf-set-batch-size
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
