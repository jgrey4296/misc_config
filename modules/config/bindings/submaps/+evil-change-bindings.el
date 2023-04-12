;;; +evil-change-bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-change-map
      :desc "Split Line"                  "RET" #'electric-newline-and-maybe-indent

      :desc "Rot13"                       "'" #'evil-rot13
      :desc "ispell-word"                 "=" #'ispell-word

      :desc "Down"                        "d" #'evil-downcase
      :desc "Decode url"                  "E" #'+evil:url-decode

      :desc "Comment"                     "c" #'evilnc-comment-operator
      :desc "Encode url"                  "e" #'+evil:url-encode
      :desc "Shift Left"                  "h" (cmd! (call-interactively (if (eq evil-state 'visual) 'evil-shift-left 'evil-shift-left-line)))
      :desc "Inflection"                  "i" #'evil-operator-string-inflection
      :desc "Shift Right"                 "l" (cmd! (call-interactively (if (eq evil-state 'visual) 'evil-shift-right 'evil-shift-right-line)))

      :desc "Surround"                    "s" #'evil-surround-region
      :desc "Upper"                       "u" #'evil-upcase

      )
