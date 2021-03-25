(map! :map global-map
      :desc "Insert Char" :gi "C-x 8 RET" #'insert-char
      )
(map! :map global-map
      :prefix "C-x 8"
      (:prefix ("*" . ""))
      (:prefix ("/" . ""))
      (:prefix ("1" . ""))
      (:prefix ("2" . ""))
      (:prefix ("3" . ""))
      (:prefix ("^" . ""))
      (:prefix ("_" . ""))
      (:prefix ("a" . ""))
      (:prefix ("N" . ""))
      (:prefix ("O" . ""))
      )
(map! :map global-map
      :prefix "C-x 8"
        :ig "!" #'ignore
        :ig "$" #'ignore
        :ig "+" #'ignore
        :ig "-" #'ignore
        :ig "<" #'ignore
        :ig "=" #'ignore
        :ig ">" #'ignore
        :ig "?" #'ignore
        :ig "[" #'ignore
        :ig "]" #'ignore
        :ig "c" #'ignore
        :ig "C" #'ignore
        :ig "L" #'ignore
        :ig "o" #'ignore
        :ig "P" #'ignore
        :ig "R" #'ignore
        :ig "u" #'ignore
        :ig "m" #'ignore
        :ig "x" #'ignore
        :ig "Y" #'ignore
        :ig "{" #'ignore
        :ig "|" #'ignore
        :ig "}" #'ignore
      )
;; Acute
(map! :map global-map
      :prefix ("C-x 8 '" . "Acute")
      :desc "´" :ig "'" "´"
      :desc "á" :ig "a" "á"
      :desc "ć" :ig "c" "ć"
      :desc "é" :ig "e" "é"
      :desc "ǵ" :ig "g" "ǵ"
      :desc "í" :ig "i" "í"
      :desc "ḱ" :ig "k" "ḱ"
      :desc "ĺ" :ig "l" "ĺ"
      :desc "ḿ" :ig "m" "ḿ"
      :desc "ń" :ig "n" "ń"
      :desc "ó" :ig "o" "ó"
      :desc "ṕ" :ig "p" "ṕ"
      :desc "ŕ" :ig "r" "ŕ"
      :desc "ś" :ig "s" "ś"
      :desc "ú" :ig "u" "ú"
      :desc "ẃ" :ig "w" "ẃ"
      :desc "ý" :ig "y" "ý"
      :desc "ź" :ig "z" "ź"
      )
;; Cedilla
(map! :map global-map
      :prefix ("C-x 8 ," . "Cedilla")
      :desc "ç" :ig "c"  "ç"
      :desc "ḑ" :ig "d"  "ḑ"
      :desc "ȩ" :ig "e"  "ȩ"
      :desc "ģ" :ig "g"  "ģ"
      :desc "ḩ" :ig "h"  "ḩ"
      :desc "ķ" :ig "k"  "ķ"
      :desc "ļ" :ig "l"  "ļ"
      :desc "ņ" :ig "n"  "ņ"
      :desc "ŗ" :ig "r"  "ŗ"
      :desc "ş" :ig "s"  "ş"
      :desc "ţ" :ig "t"  "ţ"
      )
;; Diaresis
(map! :map global-map
      :prefix ("C-x 8 \"" . "Diaeresis")
      :desc "ä" :ig "a"  "ä"
      :desc "ë" :ig "e"  "ë"
      :desc "ḧ" :ig "h"  "ḧ"
      :desc "ï" :ig "i"  "ï"
      :desc "ö" :ig "o"  "ö"
      :desc "ẗ" :ig "t"  "ẗ"
      :desc "ü" :ig "u"  "ü"
      :desc "ẅ" :ig "w"  "ẅ"
      :desc "ẍ" :ig "x"  "ẍ"
      :desc "ÿ" :ig "y"  "ÿ"
      )
;; Greek
(map! :map global-map
      :prefix ("C-x 8 g" . "Greek")
      ;; Greek Letters
      :desc "α" :ig "a"  "α"
      :desc "β" :ig "b"  "β"
      :desc "γ" :ig "c"  "γ"
      :desc "δ" :ig "d"  "δ"
      :desc "ε" :ig "e"  "ε"
      :desc "ζ" :ig "f"  "ζ"
      :desc "η" :ig "g"  "η"
      :desc "θ" :ig "h"  "θ"
      :desc "ι" :ig "i"  "ι"
      :desc "κ" :ig "k"  "κ"
      :desc "λ" :ig "l"  "λ"
      :desc "μ" :ig "m"  "μ"
      :desc "ν" :ig "n"  "ν"
      :desc "ξ" :ig "x"  "ξ"
      :desc "ο" :ig "o"  "ο"
      :desc "π" :ig "p"  "π"
      :desc "ρ" :ig "r"  "ρ"
      :desc "σ" :ig "s"  "σ"
      :desc "τ" :ig "t"  "τ"
      :desc "υ" :ig "u"  "υ"
      :desc "φ" :ig "p"  "φ"
      :desc "χ" :ig "x"  "χ"
      :desc "ψ" :ig "y"  "ψ"
      :desc "ω" :ig "z"  "ω"
      )
;; Grave
(map! :map global-map
      :prefix ("C-x 8 `" . "Grave")
      :desc "à" :ig "a"  "à"
      :desc "è" :ig "e"  "è"
      :desc "ì" :ig "i"  "ì"
      :desc "ǹ" :ig "n"  "ǹ"
      :desc "ò" :ig "o"  "ò"
      :desc "ù" :ig "u"  "ù"
      :desc "ẁ" :ig "w"  "ẁ"
      :desc "ỳ" :ig "y"  "ỳ"
)
;; Logic
(map! :map global-map
      :prefix ("C-x 8 l" . "Logic")
      :desc "∀" :ig "a"  "∀"
      :desc "∃" :ig "E"  "∃"
      :desc "∄" :ig "N"  "∄"
      :desc "∈" :ig "e"  "∈"
      :desc "¬" :ig "n"  "¬"
      :desc "∨" :ig "d"  "∨"
      :desc "∧" :ig "c"  "∧"
      :desc "⧼" :ig "<"  "⧼"
      :desc "⧼" :ig ">"  "⧼"
      :desc "⇒" :ig "i"  "⇒"
      :desc "⇔" :ig "I"  "⇔"
      :desc "⟙" :ig "t"  "⟙"
      :desc "⟘" :ig "b"  "⟘"
      :desc "⊨" :ig "\\" "⊨"
      :desc "⊢" :ig "/"  "⊢"
      :desc "∴" :ig "T"  "∴"
      :desc "□" :ig "["  "□"
      :desc "◇" :ig "]"  "◇"
      :desc "⚬" :ig "o"  "⚬"
      )
;; Math
(map! :map global-map
      :prefix ("C-x 8 M" . "Math")
      :desc "⊂" :ig "s" "⊂"
      :desc "⊃" :ig "S" "⊃"
      :desc "⊆" :ig "e" "⊆"
      :desc "⊇" :ig "E" "⊇"
      :desc "∅" :ig "n" "∅"
      :desc "∩" :ig "i" "∩"
      :desc "∪" :ig "u" "∪"
      ;; Math MisC
      :desc "√" :ig "q" "√"
      :desc "∞" :ig "8" "∞"
      ;; Fractions
      )
;; Subscript
(map! :map global-map
      :prefix ("C-x 8 s" . "Subscript")
      :desc "₁" :ig "1" "₁"
      :desc "₂" :ig "2" "₂"
      :desc "₃" :ig "3" "₃"
      :desc "₄" :ig "4" "₄"
      :desc "₅" :ig "5" "₅"
      :desc "₆" :ig "6" "₆"
      :desc "₇" :ig "7" "₇"
      :desc "₈" :ig "8" "₈"
      :desc "₉" :ig "9" "₉"
      :desc "₀" :ig "0" "₀"
      :desc "ⱼ" :ig "j" "ⱼ"
      :desc "ᵢ" :ig "i" "ᵢ"
      :desc "₊" :ig "+" "₊"
      :desc "₋" :ig "-" "₋"
      )
;; Superscript
(map! :map global-map
      :prefix ("C-x 8 S" . "Superscript")

      )
;; Tilde
(map! :map global-map
      :prefix ("C-x 8 ~" . "Tilde")
      :desc "ã" :ig "a"  "ã"
      :desc "ẽ" :ig "e"  "ẽ"
      :desc "ĩ" :ig "i"  "ĩ"
      :desc "ñ" :ig "n"  "ñ"
      :desc "õ" :ig "o"  "õ"
      :desc "ũ" :ig "u"  "ũ"
      :desc "ṽ" :ig "v"  "ṽ"
      :desc "ỹ" :ig "y"  "ỹ"
)


