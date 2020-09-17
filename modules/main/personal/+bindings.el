
(global-set-key (kbd "C-c [") 'jg-spacemacs-main-layer/insert-lparen)
(global-set-key (kbd "C-c ]") 'jg-spacemacs-main-layer/insert-rparen)

;; TODO
;; (spacemacs/declare-prefix "x i" "Indent")
;; (spacemacs/declare-prefix "a m" "Modes")
;; (spacemacs/declare-prefix "a f" "Free Keys")
;; (spacemacs/set-leader-keys "h s" 'describe-syntax)
(map! :prefix "z '"
      :n "i" 'hi-lock-write-interactive-patterns
      :n "u" 'unhighlight-regexp
      :n "." 'highlight-symbol-at-point
      :n "r" 'highlight-regexp
      :n "p" 'highlight-phrase
      :n "l" 'highlight-lines-matching-regexp
      :n "f" 'hi-lock-find-patterns
      )

