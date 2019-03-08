
(require 'generic-x)

(define-generic-mode
    'trie-mode                     ;; name of the mode
  '("//")                          ;; comments delimiter
  '("function" "var" "return")     ;; keywords
  '(("=" . 'font-lock-operator)    ;;
    ("+" . 'font-lock-operator)
    (";" . 'font-lock-builtin))    ;; a built in
  '("\\.trie$")                    ;; file regex to trigger mode
  nil                              ;; other functions to call
  "My custom trie highlighting mode"
  )

;;To use with (require 'trie-mode):
;;(provide 'trie-mode)
