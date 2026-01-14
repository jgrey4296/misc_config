;; Leader no prefix
;;
(dlog! "Setting up leader bindings: %s" (current-time-string))

(local-load! "+bind-leader")
(local-load! "+bind-global")

(map! :map jgb-change--root-map
      (:prefix ("b" . "Buffer"))
)

(provide 'jg-bindings-core)
