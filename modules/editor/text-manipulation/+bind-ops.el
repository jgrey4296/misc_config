;;; +bind-ops.el -*- lexical-binding: t; no-byte-compile: t; -*-


(map! :map jge-operator-map
      :desc "Complete/Grow Selection"     "g" (cmds! (eq evil-state 'normal) #'company-manual-begin
                                                     (eq evil-state 'visual) #'+jg-text-grow-selection-op)
      :desc "Yank"                        "y" #'+evil:yank-unindented
      :desc "Regexp Builder"               "R"    #'regexp-builder
)

(map! :map jge-operator-map :prefix ("d" . "Describe")
      "g" #'writegood-grade-level
      "r" #'writegood-reading-ease
      )

(map! :map jge-operator-map :prefix ("w" . "whitespace")
      :desc "Whitespace clean"            "w" #'+jg-text-run-whitespace-cleanup
      :desc "Delete trailing whitespace"  "W" #'delete-trailing-whitespace
      :desc "Whitespace Cleanup"          "c" #'whitespace-cleanup
      )

(map! :map jge-operator-map :prefix ("f" . "filter")
      :desc "Flush Lines"                "f"   #'flush-lines
      :desc "Keep Lines"                 "k"   #'keep-lines
      :desc "Uniquify"                   "u"   #'delete-duplicate-lines
      :desc "Untabify"                   "TAB" #'untabify
      )

(map! :map jge-operator-map :prefix ("/" . "Search")
      :desc "Simple Grep"          "g" #'+jg-text-simple-grep-op
      :desc "Next Similar String " "s" #'+jg-text-next-similar-string
      )


;;; +bind-ops.el ends here
