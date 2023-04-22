;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map json-mode-map
      :localleader
      :desc "Copy path" "p" #'json-mode-show-path
      "d" #'json-mode-kill-path
      "x" #'json-nullify-sexp
      "f" #'json-mode-beautify
      "s" #'counsel-jq)
