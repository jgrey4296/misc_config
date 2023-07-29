;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :prefix "c"
      :desc "Evaluate & replace region"            "E"     #'+eval:replace-region
      :desc "Evaluate buffer/region"               "e"     #'+eval/buffer-or-region
      :desc "Send to repl"                         "s"     #'+jg-send-region-to-repl
      :desc "Clear"                                "r c"   #'+jg-repl-clear
      :desc "Open Repl"                            "r o"   #'+jg-eval-open-repl
      :desc "Send to Repl"                         "r r"   #'+jg-repl-send-register-to-repl

)
