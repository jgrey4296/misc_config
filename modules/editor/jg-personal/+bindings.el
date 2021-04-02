(message "Loading modules/editor/jg-personal/+bindings.el")

(map! :mode vlf-mode
      "] A" 'vlf-next-batch-from-point
      "] a" 'vlf-next-batch
      "[ a" 'vlf-prev-batch
      "SPC a U v " 'vlf-set-batch-size
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

(map! :leader
      :prefix "t"
      :desc "Semantic" "S" #'semantic-mode
      )

;; todo add semantic etc
