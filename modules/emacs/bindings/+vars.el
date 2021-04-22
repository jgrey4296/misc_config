;;; util/bindings/+vars.el -*- lexical-binding: t; -*-

(setq-default jg-google-url "https://duckduckgo.com/?q=%s"
              jg-twitter-url "https://twitter.com"
              default-input-method "greek"
)


(setq jg-binding-operator-map           (make-sparse-keymap "evil operators")
      jg-binding-vision-map             (make-sparse-keymap "vision manipulation")
      jg-binding-forward-motion-map     (make-sparse-keymap "forward motion")
      jg-binding-backward-motion-map    (make-sparse-keymap "backward motion")
      jg-binding-inner-text-objects-map (make-sparse-keymap "inner textobjs")
      jg-binding-outer-text-objects-map (make-sparse-keymap "outer textobjs"))



