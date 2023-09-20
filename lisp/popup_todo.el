;; todo.el -*- lexical-binding: t; -*-

(setq popup (popup-create (point) 10 10))
(popup-set-list popup '("Foo" "Bar" "Baz"))
(popup-draw popup)
;; do something here
(popup-delete popup)

(popup-tip "Hello, World!")


(popup-menu* '("Foo" "Bar" "Baz"))
;; => "Baz" if you select Baz
(popup-menu* (list (popup-make-item "Yes" :value t)
                   (popup-make-item "No" :value nil)))
;; => t if you select Yes

(popup-cascade-menu '(("Top1" "Sub1" "Sub2") "Top2"))
