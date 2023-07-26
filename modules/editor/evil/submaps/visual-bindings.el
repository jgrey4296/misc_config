;;; +evil-visual-state-bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-visual-state-map ;; enter/exit
      [escape] 'evil-normal-state
      :prefix ("v" . "Visual")
      :desc "buffer"       "RET"           #'mark-whole-buffer
      :desc "line"         "j"             #'evil-visual-line
      :desc "Block"        "k"             #'evil-visual-block
      :desc "char"         "l"             #'evil-visual-char
      :desc "exit"         "v"             #'evil-normal-state
      )
(map! :map jg-binding-visual-state-map
      :desc "Replace Selection"       "R"   #'evil-change
      :desc "Exchange Corners"        "A"   #'evil-visual-exchange-corners
      :desc "Exchange Point and Mark" "a"   #'exchange-point-and-mark

      :desc "exit"    "V"                   #'evil-exit-visual-state

      :desc "Indent"                  "TAB" #'indent-for-tab-command
      :desc "Macro"                   "@"   #'+evil:apply-macro
      :desc "L-Shift"                 "<"   #'+evil/shift-left
      :desc "R-Shift"                 ">"   #'+evil/shift-right
      :desc "Search"                  "/"   #'evil-ex-search-forward
      :desc "Visual Search"           "?"   #'evil-visualstar/begin-search-forward

      :desc "Paste Over"              "p"   #'evil-visual-paste
      :desc "Yank"                    "y"   #'evil-yank
      )
