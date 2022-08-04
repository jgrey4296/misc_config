
(message "Setting up char insert bindings: %s" (current-time-string))
(defvar char-insert-cx8-map         (make-sparse-keymap))
(defvar char-insert-acute-map       (make-sparse-keymap))
(defvar char-insert-cedilla-map     (make-sparse-keymap))
(defvar char-insert-diaeresis-map   (make-sparse-keymap))
(defvar char-insert-greek-map       (make-sparse-keymap))
(defvar char-insert-grave-map       (make-sparse-keymap))
(defvar char-insert-logic-map       (make-sparse-keymap))
(defvar char-insert-math-map        (make-sparse-keymap))
(defvar char-insert-subscript-map   (make-sparse-keymap))
(defvar char-insert-superscript-map (make-sparse-keymap))
(defvar char-insert-tilde-map       (make-sparse-keymap))


;; Acute
(map! :map char-insert-acute-map
      :desc "´"  "'" "´"
      :desc "á"  "a" "á"
      :desc "ć"  "c" "ć"
      :desc "é"  "e" "é"
      :desc "ǵ"  "g" "ǵ"
      :desc "í"  "i" "í"
      :desc "ḱ"  "k" "ḱ"
      :desc "ĺ"  "l" "ĺ"
      :desc "ḿ"  "m" "ḿ"
      :desc "ń"  "n" "ń"
      :desc "ó"  "o" "ó"
      :desc "ṕ"  "p" "ṕ"
      :desc "ŕ"  "r" "ŕ"
      :desc "ś"  "s" "ś"
      :desc "ú"  "u" "ú"
      :desc "ẃ"  "w" "ẃ"
      :desc "ý"  "y" "ý"
      :desc "ź"  "z" "ź"
      )
;; Cedilla
(map! :map char-insert-cedilla-map
      :desc "ç"  "c"  "ç"
      :desc "ḑ"  "d"  "ḑ"
      :desc "ȩ"  "e"  "ȩ"
      :desc "ģ"  "g"  "ģ"
      :desc "ḩ"  "h"  "ḩ"
      :desc "ķ"  "k"  "ķ"
      :desc "ļ"  "l"  "ļ"
      :desc "ņ"  "n"  "ņ"
      :desc "ŗ"  "r"  "ŗ"
      :desc "ş"  "s"  "ş"
      :desc "ţ"  "t"  "ţ"
      )
;; Diaresis
(map! :map char-insert-diaeresis-map
      :desc "ä"  "a"  "ä"
      :desc "ë"  "e"  "ë"
      :desc "ḧ"  "h"  "ḧ"
      :desc "ï"  "i"  "ï"
      :desc "ö"  "o"  "ö"
      :desc "ẗ"  "t"  "ẗ"
      :desc "ü"  "u"  "ü"
      :desc "ẅ"  "w"  "ẅ"
      :desc "ẍ"  "x"  "ẍ"
      :desc "ÿ"  "y"  "ÿ"
      )
;; Greek
(map! :map char-insert-greek-map
      ;; Greek Letters
      :desc "α"  "a"  "α"
      :desc "β"  "b"  "β"
      :desc "γ"  "c"  "γ"
      :desc "δ"  "d"  "δ"
      :desc "ε"  "e"  "ε"
      :desc "ζ"  "f"  "ζ"
      :desc "η"  "g"  "η"
      :desc "θ"  "h"  "θ"
      :desc "ι"  "i"  "ι"
      :desc "κ"  "k"  "κ"
      :desc "λ"  "l"  "λ"
      :desc "μ"  "m"  "μ"
      :desc "ν"  "n"  "ν"
      :desc "ξ"  "x"  "ξ"
      :desc "ο"  "o"  "ο"
      :desc "π"  "p"  "π"
      :desc "ρ"  "r"  "ρ"
      :desc "σ"  "s"  "σ"
      :desc "τ"  "t"  "τ"
      :desc "υ"  "u"  "υ"
      :desc "φ"  "p"  "φ"
      :desc "χ"  "x"  "χ"
      :desc "ψ"  "y"  "ψ"
      :desc "ω"  "z"  "ω"
      )
;; Grave
(map! :map char-insert-grave-map
      :desc "à"  "a"  "à"
      :desc "è"  "e"  "è"
      :desc "ì"  "i"  "ì"
      :desc "ǹ"  "n"  "ǹ"
      :desc "ò"  "o"  "ò"
      :desc "ù"  "u"  "ù"
      :desc "ẁ"  "w"  "ẁ"
      :desc "ỳ"  "y"  "ỳ"
      )
;; Logic
(map! :map char-insert-logic-map
      :desc "∀"  "a"  "∀"
      :desc "∃"  "E"  "∃"
      :desc "∄"  "N"  "∄"
      :desc "∈"  "e"  "∈"
      :desc "¬"  "n"  "¬"
      :desc "∨"  "d"  "∨"
      :desc "∧"  "c"  "∧"
      :desc "⧼"  "<"  "⧼"
      :desc "⧼"  ">"  "⧼"
      :desc "⇒"  "i"  "⇒"
      :desc "⇔"  "I"  "⇔"
      :desc "⟙"  "t"  "⟙"
      :desc "⟘"  "b"  "⟘"
      :desc "⊨"  "\\" "⊨"
      :desc "⊢"  "/"  "⊢"
      :desc "∴"  "T"  "∴"
      :desc "□"  "["  "□"
      :desc "◇"  "]"  "◇"
      :desc "⚬"  "o"  "⚬"
      )
;; Math
(map! :map char-insert-math-map
      :desc "⊂"  "s" "⊂"
      :desc "⊃"  "S" "⊃"
      :desc "⊆"  "e" "⊆"
      :desc "⊇"  "E" "⊇"
      :desc "∅"  "n" "∅"
      :desc "∩"  "i" "∩"
      :desc "∪"  "u" "∪"
      ;; Math MisC
      :desc "√"  "q" "√"
      :desc "∞"  "8" "∞"
      ;; Fractions
      )
;; Subscript
(map! :map char-insert-subscript-map
      :desc "₁"  "1" "₁"
      :desc "₂"  "2" "₂"
      :desc "₃"  "3" "₃"
      :desc "₄"  "4" "₄"
      :desc "₅"  "5" "₅"
      :desc "₆"  "6" "₆"
      :desc "₇"  "7" "₇"
      :desc "₈"  "8" "₈"
      :desc "₉"  "9" "₉"
      :desc "₀"  "0" "₀"
      :desc "ⱼ"  "j" "ⱼ"
      :desc "ᵢ"  "i" "ᵢ"
      :desc "₊"  "+" "₊"
      :desc "₋"  "-" "₋"
      )
;; Superscript
(map! :map char-insert-superscript-map

      )
;; Tilde
(map! :map char-insert-tilde-map
      :desc "ã"  "a"  "ã"
      :desc "ẽ"  "e"  "ẽ"
      :desc "ĩ"  "i"  "ĩ"
      :desc "ñ"  "n"  "ñ"
      :desc "õ"  "o"  "õ"
      :desc "ũ"  "u"  "ũ"
      :desc "ṽ"  "v"  "ṽ"
      :desc "ỹ"  "y"  "ỹ"
      )


;; Assemble
(map! :map char-insert-cx8-map
      :desc "Acute" "'"       char-insert-acute-map
      :desc "Cedilla" ","     char-insert-cedilla-map
      :desc "Diaeresis" "\""  char-insert-diaeresis-map
      :desc "Greek" "g"       char-insert-greek-map
      :desc "Grave" "`"       char-insert-grave-map
      :desc "Logic" "l"       char-insert-logic-map
      :desc "Math" "M"        char-insert-math-map
      :desc "Subscript" "s"   char-insert-subscript-map
      :desc "Superscript" "S" char-insert-superscript-map
      :desc "Tilde" "~"       char-insert-tilde-map
      :desc "Counsel Insert" "RET" #'insert-char
      )

;; Override
(map! :after jg-evil-bindings
      :map evil-insert-state-map
      :desc "Insert Chars" "C-x 8" char-insert-cx8-map
      )

(map! :after jg-evil-bindings
      :map evil-normal-state-map
      :desc "Insert Chars" "C-x 8" char-insert-cx8-map
      )
