;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Clear" "crc" #'+jg-repl-clear
      :desc "Send to Repl" "rr" #'+jg-repl-send-register-to-repl
)
