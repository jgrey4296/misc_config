;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Clear"        "c r c" #'+jg-repl-clear
      :desc "Open Repl"    "c r o" #'+jg-eval-open-repl
      :desc "Send to Repl" "r r"   #'+jg-repl-send-register-to-repl
)
