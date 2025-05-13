;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Eval expression"       "\""   #'pp-eval-expression
      :desc "M-x"                   ";"   #'execute-extended-command

      :prefix "c"
      :desc  "compile in project"                   "c"     #'projectile-compile-project
      :desc  "Evaluate & replace region"            "E"     #'+eval:replace-region
      :desc  "Evaluate buffer/region"               "e"     #'+jg-eval-run-buffer
      :desc  "Send to repl"                         "s"     #'+jg-send-region-to-repl
      :desc  "quickrun"                             "q"     #'quickrun
      (:prefix ("r" . "Repls")
       :desc "Clear"                                "c"   #'+jg-repl-clear
       :desc "Open Repl"                            "o"   #'+jg-eval-open-repl
       :desc "Send to Repl"                         "r"   #'+jg-repl-send-register-to-repl
       )
      )
