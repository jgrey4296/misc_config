
(map! :map csharp-mode-map
      :desc "General Insert"         :n "|" #'general-insert-call

      )

(map! :map csharp-mode-map
      :localleader
)

(map! :map fsharp-mode-map
      :localleader
      )

(map! :localleader
      :map fsharp-mode-map
      "b" #'fsharp-ac/pop-gotodefn-stack ; Useful for re-tracing your steps
      "e" #'fsharp-eval-region
      "l" #'fsharp-load-buffer-file
      (:unless (modulep! +lsp)
        "q" #'fsharp-ac/stop-process
        "t" #'fsharp-ac/show-tooltip-at-point)
      )

(map! :map jg-dired-mode-map
      :desc "Disassemble Dotnet"    :n "d ? d" #'+jg-dotnet-dired-ilspy
      )
