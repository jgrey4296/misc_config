;; TODO
;; (spacemacs/declare-prefix "x i" "Indent")
;; (spacemacs/declare-prefix "a m" "Modes")
(after! hi-lock
  (map! :map evil-normal-state-map
        :prefix "z '"
        "i" 'hi-lock-write-interactive-patterns
        "u" 'unhighlight-regexp
        "." 'highlight-symbol-at-point
        "r" 'highlight-regexp
        "p" 'highlight-phrase
        "l" 'highlight-lines-matching-regexp
        "f" 'hi-lock-find-patterns
        )
)
