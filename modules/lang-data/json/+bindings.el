;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map json-mode-map
      :localleader
      :desc "Copy path" "p" #'json-mode-show-path
      "t" #'json-toggle-boolean
      "d" #'json-mode-kill-path
      "x" #'json-nullify-sexp
      "+" #'json-increment-number-at-point
      "-" #'json-decrement-number-at-point
      "f" #'json-mode-beautify
      "s" #'counsel-jq)
