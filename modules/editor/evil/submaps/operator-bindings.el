;;; +op-bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-operator-state-map
      [escape] 'evil-normal-state
      :desc "Escape"                 "C-g"     #'evil-escape
      :desc "EOL"                    "$"       #'evil-end-of-visual-line
      :desc "BOL"                    "0"       #'evil-beginning-of-visual-line
      :desc "line start"             "k"       #'evil-beginning-of-line
      :desc "Line End"               "j"       #'evil-end-of-line
      :desc "Back Char"              "h"       #'evil-backward-char
      :desc "Forward Char"           "l"       #'evil-forward-char
      )
(map! :map jg-binding-backward-operator-motion-map
      ;; RET ] l r
      :desc "Section"      "["   #'evil-backward-section-begin
      :desc "Arg"          "a"   #'evil-backward-arg
      :desc "Begin Method" "m"   #'+evil/previous-beginning-of-method
      :desc "End Method"   "M"   #'+evil/previous-end-of-method
      :desc "Section"      "s"   #'evil-backward-section-begin
      )
(map! :map jg-binding-forward-operator-motion-map
      ;; r RET l [
      :desc "Section"      "]" #'evil-forward-section-begin
      :desc "Arg"          "a" #'evil-forward-arg
      :desc "Heading"      "h" #'outline-next-visible-heading
      :desc "Begin Method" "m" #'+evil/next-beginning-of-method
      :desc "End Method"   "M" #'+evil/next-end-of-method
      :desc "Section"      "s" #'evil-forward-section-begin
      )
(map! :map jg-binding-operator-map
      ;; g > s
      :desc "Repeat Global Sub"   "7"   #'evil-ex-repeat-global-substitute

      :desc "Apply Macro"         "@"   #'+evil:apply-macro
      :desc "Char"                "?"   #'what-cursor-position

      :desc "IEdit"              "e" #'iedit-mode
      )
