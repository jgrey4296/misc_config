(map! :map global-map
      :prefix "C-x 8"
      (:prefix ("g" . "Greek")
       ;; Greek Letters
       "a" #'alpha-char
       "b" #'beta-char
       "c" #'gamma-char
       "d" #'delta-char
       "e" #'epsilon-char
       "f" #'zeta-char
       "g" #'eta-char
       "h" #'theta-char
       "i" #'iota-char
       "k" #'kappa-char
       "l" #'lambda-char
       "m" #'mu-char
       "n" #'nu-char
       "x" #'xi-char
       "o" #'omicron-char
       "p" #'pi-char
       "r" #'rho-char
       "s" #'sigma-char
       "t" #'tau-char
       "u" #'upsilon-char
       "p" #'phi-char
       "x" #'chi-char
       "y" #'psi-char
       "z" #'omega-char

       ;; Capital Greek
       "A" #'ualpha-char
       "B" #'ubeta-char
       "C" #'ugamma-char
       "D" #'udelta-char
       "E" #'uepsil-char
       "F" #'uzeta-char
       "G" #'ueta-char
       "H" #'utheta-char
       "I" #'uiota-char
       "K" #'ukappa-char
       "L" #'ulambda-char
       "M" #'umu-char
       "N" #'unu-char
       "X" #'uxi-char
       "O" #'uomicr-char
       "P" #'upi-char
       "R" #'urho-char
       "S" #'usigma-char
       "T" #'utau-char
       "U" #'uupsil-char
       "P" #'uphi-char
       "X" #'uchi-char
       "Y" #'upsi-char
       "Z" #'uomega-char
      )

       ;; Subscript
       (:prefix ("s" . "Subscript")
        "1" #'sub-1-char
        "2" #'sub-2-char
        "3" #'sub-3-char
        "4" #'sub-4-char
        "5" #'sub-5-char
        "6" #'sub-6-char
        "7" #'sub-7-char
        "8" #'sub-8-char
        "9" #'sub-9-char
        "0" #'sub-0-char
        "n" #'sub-n-char
        "j" #'sub-j-char
        "i" #'sub-i-char
        "k" #'sub-k-char
        "m" #'sub-m-char
        "+" #'sub-plus-char
        "-" #'sub-minus-char
)
                  ;; Math - Sets
       (:prefix ("M" . "Math")
        "s" #'subset-char
        "S" #'superset-char
        "e" #'subset-eq-char
        "E" #'superset-eq-char
        "n" #'null-char
        "i" #'intersect-char
        "u" #'union-char
        ;; Math MisC
        "q" #'sqroot-char
        "8" #'infinity-char
        )

       ;; Logic
       (:prefix ("l" . "Logic")
        "a" #'forall-char
        "E" #'exists-char
        "N" #'not-exists-char
        "e" #'element-char
        "n" #'not-char
        "d" #'or-char
        "c" #'and-char
        "<" #'l-angle-bracket-char
        ">" #'r-angle-bracket-char
        "i" #'implies-char
        "I" #'bi-condition-char
        "t" #'top-char
        "b" #'bottom-char
        "\\" #'turnstile-char
        "/" #'satisfies-char
        "T" #'therefore-char
        "[" #'box-char
        "]" #'diamond-char
        "o" #'circle-char
        )
)
