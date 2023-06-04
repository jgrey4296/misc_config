;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map json-mode-map
      :localleader
      :desc "Copy path" "p" #'json-mode-show-path
      "d" #'json-mode-kill-path
      "x" #'json-nullify-sexp
      "f" #'json-mode-beautify
      "s" #'counsel-jq)

(map! :map jg-dired-mode-map
      (:prefix ("> j" . "jq")
       :desc "Format" "f" #'+jg-json-jq-format
       :desc "Manual" "?" (cmd! (+jg-browse-url "https://stedolan.github.io/jq/manual/"))
       :desc "Expr"   "e" #'+jg-json-jq-expr
       )

      :desc "Reformat jsons"      :n "cJ" #'+jg-dired-reformat-jsons
      )
