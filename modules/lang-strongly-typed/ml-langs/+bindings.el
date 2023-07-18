;;; +bindings.el -*- lexical-binding: t; -*-


  (map! :localleader
        :map tuareg-mode-map
        "a" #'tuareg-find-alternate-file
        "t" #'merlin-type-enclosing
        "R" #'merlin-iedit-occurrences
        )

(map! :map sml-mode-map
        :i "RET"   #'reindent-then-newline-and-indent
        :i "S-SPC" #'sml-electric-space
        :i "|"     #'sml-electric-pipe
        :localleader
        :desc "Run SML" "'" #'run-sml
        :prefix ("e" . "eval")
        :desc "Run buffer"                  "b" #'sml-prog-proc-send-buffer
        :desc "Run the paragraph"           "f" #'sml-send-function
        :desc "Run region"                  "r" #'sml-prog-proc-send-region
        )
