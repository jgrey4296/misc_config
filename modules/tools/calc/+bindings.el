;;; calc-bindings.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header


(map! :map jg-calc-mode-map ;; prefixs
      (:prefix ("a" . "algebra"))
      (:prefix ("f" . "funcs"))
      (:prefix ("m" . "modes"))
      (:prefix ("s" . "State"))
      (:prefix ("v" . "select"))
      (:prefix ("V". "Vectors"))
      )

(map! :map jg-calc-mode-map ;; basic

      :n "'"        'calc-algebraic-entry
      :n "\""       'calc-edit-selection
      :n "`"        'calc-edit
      :n "q"        'calc-quit
      :n "u"        'calc-undo
      :n "U"        'calc-redo
      :n "y"        'calc-yank
      :n "I"        'calc-inverse
      :n "H"        'calc-hyperbolic
      :n "O"        'calc-option
      :n "c"        'calc-clean
      :n "C"        'calc-clean-num

      :n "+"        'calc-plus
      :n "-"        'calc-minus
      :n "*"        'calc-times
      :n "/"        'calc-divide
      :n ":"        'calc-fdiv
      :n ";"        'calc-semi
      :n "%"        'calc-mod
      :n "&"        'calc-inv
      :n "^"        'calc-power

      ;; TODO evil-calc-complex-state
      :n "("        'calc-begin-complex
      :n ")"        'calc-end-complex
      :n ","        'calc-comma

      :n "RET"      'calc-enter
      :n "="        'calc-evaluate
      ;; calc-edit-EvalRules
      :n "DEL"      'calc-pop
      ;; :n "DEL"      'calc-pop-above
      :n "?"        'calc-help

      ;; :n "Y"        'calc-shift-Y-prefix-help
      :n "o"        'calc-over
      :n "k"        'calc-roll-up
      :n "j"        'calc-roll-down
      :n "l"        'calc-last-args-stub
      :n "-"        'calc-change-sign

      ;; calc-copy-special-constant
      ;; calc-remove-duplicates
      :n "~" 'calc-num-prefix

      ;; calc-increment
      ;; calc-decrement
      ;; calc-kill
      ;; calc-kill-region
      ;; calc-keep-args
      ;; calc-last-args
      )
(map! :map jg-calc-mode-map ;; numbers
      :n "e" 'calcDigit-start
      :n "." 'calcDigit-start
      :n "#" 'calcDigit-start
      :n "@" 'calcDigit-start
      (:prefix ("i" . "insert")
               :n "p" 'calc-pi
        )
      ;; :n "0" 'calcDigit-start
      ;; :n "1" 'calcDigit-start
      ;; :n "2" 'calcDigit-start
      ;; :n "3" 'calcDigit-start
      ;; :n "4" 'calcDigit-start
      ;; :n "5" 'calcDigit-start
      ;; :n "6" 'calcDigit-start
      ;; :n "7" 'calcDigit-start
      ;; :n "8" 'calcDigit-start
      ;; :n "9" 'calcDigit-start
      )

(map! :map jg-calc-map      ;; algebra
      :prefix ("A" . "Algebra")
      ;; "\"" 'calc-auto-algebraic-entry
      ;; "$"  'calc-auto-algebraic-entry

      ;; calc-summation
      ;; calc-alt-summation
      ;; calc-product
      ;; calc-tabulate
      ;; calc-let
      ;; calc-solve-for
      ;; calc-simplify
      ;; calc-simplify-extended
      ;; calc-simplify-units
      ;; calc-sel-evaluate
      ;; calc-sel-expand-formula
      ;; calc-expand
      ;; calc-expand-formula
      ;; calc-alg-evaluate
      ;; calc-eval-num
      ;; calc-evalto
      ;; calc-assign
      ;; calc-collect
      ;; calc-normalize-rat

      ;; calc-change-sign
      ;; calc-sel-negate
      ;; calc-sel-invert
      ;; calc-commute-left
      ;; calc-commute-right
      ;; calc-sel-commute
      ;; calc-sel-distribute
      ;; calc-factor
      ;; calc-sel-isolate
      ;; calc-sel-merge
      ;; calc-sel-unpack
      ;; calc-map-equation
      ;; calc-substitute

      ;; calc-sel-add-both-sides
      ;; calc-sel-mult-both-sides
      ;; calc-sel-sub-both-sides
      ;; calc-sel-div-both-sides
      ;; calc-sel-jump-equals

      ;; calc-rewrite
      ;; calc-rewrite-selection
      ;; calc-match
      )
(map! :map jg-calc-mode-map ;; functions
      :prefix ("f" . "funcs")
      (:prefix ("f" . "financial")
       :n "c" 'calc-percent-change
       :n "C" 'calc-convert-percent
       :n "p" 'calc-percent
       ;; calc-fin-ddb
       ;; calc-fin-fv
       ;; calc-fin-irr
       ;; calc-fin-nper
       ;; calc-fin-npv
       ;; calc-fin-pmt
       ;; calc-fin-pv
       ;; calc-fin-rate
       ;; calc-fin-syd
       ;; calc-fin-sln
       )
      (:prefix ("t" . "trig")
       :n "c" 'calc-cos
       :n "s" 'calc-sin
       :n "t" 'calc-tan
       :n "a" 'calc-arctan2
       :n "h" 'calc-hypot
       :n "b" 'calc-sincos

       )
      (:prefix ("r" . "random")
                     ;; calc-beta
                     ;; calc-gamma
                     ;; calc-inc-gamma
                     ;; calc-inc-beta
                     ;; calc-erf
                     ;; calc-bessel-J
                     ;; calc-bessel-Y
                     ;; calc-utpb
                     ;; calc-utpc
                     ;; calc-utpf
                     ;; calc-utpn
                     ;; calc-utpp
                     ;; calc-utpt
                     ;; calc-random
                     ;; calc-random-again
                     ;; calc-shuffle
             )
      (:prefix ("/" . "fraction")

             )
      (:prefix ("C" . "calculus")
       :n "i" 'calc-integral
       :n "I" 'calc-num-integral
      ;; calc-derivative
       ;; calc-taylor
       )
      (:prefix ("c" . "combinatorial")
       :n "!"        'calc-factorial
       ;; calc-choose
       ;; calc-extended-gcd
       ;; calc-gcd
       ;; calc-lcm
       ;; calc-double-factorial
       ;; calc-perm
       :n "b" 'calc-bernoulli-number
       :n "e" 'calc-euler-number
       :n "s" 'calc-stirling-number
      ;; calc-prime-factors
      ;; calc-prime-test
      ;; calc-next-prime
      ;; calc-prev-prime
      ;; calc-totient
      ;; calc-moebius
       )
      (:prefix ("b" . "binary")
       ;; calc-clip
       :n "d" 'calc-diff
      ;; calc-[xor, and, or]
      ;; calc-lshift-arith
      ;; calc-lshift-binary
      ;; calc-not
      ;; calc-pack-bits
      ;; calc-rotate-binary
      ;; calc-rshift-arith
      ;; calc-rshift-binary
      ;; calc-shift-prefix
      ;; calc-unpack-bits
       )
      (:prefix ("L" . "logical")
               ;; calc-logical-and
               ;; calc-logical-if
               ;; calc-logical-not
               ;; calc-logical-or
               ;; calc-remove-equal
               ;; calc-not-equal-to
      ;; calc-less-equal
      ;; calc-less-than
      ;; calc-equal-to
      ;; calc-greater-equal
      ;; calc-greater-than
               )
      (:prefix ("l" . "logarithmic")
       ;; calc-exp
       ;; calc-ilog
       ;; calc-expm1
       ;; calc-lnp1
       ;; calc-lu-quant
       ;; calc-lu-times
       ;; calc-np
       :n "l" 'calc-log
       :n "L" 'calc-ln

       )
      (:prefix ("i" . "imaginary")
               ;; calc-argument
               ;; calc-im
               ;; calc-re
               ;; calc-pack
               ;; calc-unpack
               ;; calc-conj
              )
      (:prefix ("u" . "units")
               ;; calc-db
               ;; calc-autorange-units
               ;; calc-base-units
               ;; calc-convert-exact-units
               ;; calc-convert-temperature
               ;; calc-convert-temperature
               ;; calc-convert-time-zones
               ;; calc-convert-units
               ;; calc-date
               ;; calc-date-part
               ;; calc-define-unit
               ;; calc-edit-units
               ;; calc-enter-units-table
               ;; calc-explain-units
               ;; calc-extract-units
               ;; calc-freq
               ;; calc-get-unit-definition
               ;; calc-inc-month
               ;; calc-julian
               ;; calc-mant-part
               ;; calc-midi
               ;; calc-new-month
               ;; calc-new-week
               ;; calc-new-year
               ;; calc-now
               ;; calc-permanent-units
               ;; calc-polar
               ;; calc-quick-units
               ;; calc-remove-units
               ;; calc-simplify-units
               ;; calc-spn
               ;; calc-time-zone
               ;; calc-to-degrees
               ;; calc-to-hms
               ;; calc-to-radians
               ;; calc-undefine-unit
               ;; calc-unix-time
               ;; calc-view-units-table
               ;; calc-xpon-part
               )
      (:prefix ("v" . "vector")
               ;; calc-vector-geometric-mean
               ;; calc-reduce
               ;; calc-accumulate
               ;; calc-outer-product
               ;; calc-inner-product
               ;; calc-cross
               ;; calc-vector-max
               ;; calc-vector-mean
               ;; calc-vector-min
               ;; calc-vector-product
               ;; calc-sort
               ;; calc-rnorm
               ;; calc-in-set
               ;; calc-remove-duplicates
               ;; calc-cnorm
               ;; calc-abs
               ;; calc-conj-transpose
               ;; calc-norm
               ;; calc-cross
               ;; calc-inv
               ;; calc-mdet
               ;; calc-mlud
               ;; calc-mtrace
               ;; calc-kron
               ;; calc-conj
               ;; calc-set-cardinality
               ;; calc-map
               ;; calc-mdet
               ;; calc-mlud
               ;; calc-mtrace
               ;; calc-vector-count
               ;; calc-vector-covariance
               ;; calc-set-complement
               ;; calc-set-difference
               ;; calc-set-enumerate
               ;; calc-set-intersect
               ;; calc-set-span
               ;; calc-set-union
               ;; calc-set-xor
               ;; calc-set-floor
               ;; calc-apply
               )
      (:prefix ("e" . "eval")
                           ;; calc-find-maximum
                           ;; calc-find-minimum
                           ;; calc-find-root
                           ;; calc-curve-fit
                           )
      (:prefix ("p" . "polynomial")
                     ;; calc-apart
                     ;; calc-poly-div
                     ;; calc-poly-div-rem
                     ;; calc-poly-gcd
                     ;; calc-poly-interp
                     ;; calc-poly-rem
                     ;; calc-poly-roots
                     )

      :n "a" 'calc-abs
      :n "_" 'calc-floor
      :n "R" 'calc-round
      :n "s" 'calc-sqrt
      :n "S" 'calc-isqrt
      ;; calc-abssqr

      ;; calc-scale-float
      ;; calc-concat
      ;; calc-ident
      ;; calc-idiv
      ;; calc-max
      ;; calc-min
      ;; calc-power
      ;; calc-sign
      )

(map! :map jg-calc-mode-map ;; modes
      :prefix ("m" . "modes")
      :n "a"    'calc-algebraic-mode
      :n "0"    'calc-no-simplify-mode
      :n "1"    'calc-num-simplify-mode
      :n "2"    'calc-basic-simplify-mode
      :n "3"    'calc-bin-simplify-mode
      :n "4"    'calc-alg-simplify-mode
      :n "5"    'calc-ext-simplify-mode
      :n "6"    'calc-units-simplify-mode
      :n "RET"  'calc-default-simplify-mode

      :n "d"    'calc-degrees-mode
      :n "r"    'calc-radians-mode
      :n "p"    'calc-polar-mode
      :n "f"    'calc-frac-mode
      :n "h"    'calc-hms-mode

      ;; :n " " 'calc-embedded-preserve-modes
      ;; :n " "    'calc-get-modes
      ;; :n " "    'calc-save-modes
      :n "i"    'calc-infinite-mode
      :n "m"    'calc-matrix-mode
      :n "s"    'calc-symbolic-mode
      :n "A"    'calc-total-algebraic-mode

      (:prefix ("r" . "radix")
      :n "d"    'calc-decimal-radix
      :n "b"    'calc-binary-radix
      :n "h"    'calc-hex-radix
      :n "o"    'calc-octal-radix
      ;; calc-radix
      ;; calc-word-size
      )
      (:prefix ("l" . "language")
               ;;  A               calc-giac-language
               ;;  B               calc-big-language
               ;;  C               calc-c-language
               ;;  D               calc-redo
               ;;  E               calc-eqn-language
               ;;  F               calc-fortran-language
               ;;  L               calc-latex-language
               ;;  M               calc-mathematica-language
               ;;  N               calc-normal-language
               ;;  O               calc-flat-language
               ;;  P               calc-pascal-language
               ;;  T               calc-tex-language
               ;;  U               calc-unformatted-language
               ;;  W               calc-maple-language
               ;;  X               calc-maxima-language
               ;;  Y               calc-yacas-language
               )
      (:prefix ("n" . "notation")
               ;;  c               calc-complex-notation
               ;;  d               calc-date-notation
               ;;  e               calc-eng-notation
               ;;  f               calc-fix-notation
               ;;  h               calc-hms-notation
               ;;  i               calc-i-notation
               ;;  j               calc-j-notation
               ;;  n               calc-normal-notation
               ;;  o               calc-over-notation
               ;;  s               calc-sci-notation
               )
      )

(map! :map jg-calc-mode-map ;; state
      :prefix  ("s" . "State")
      ;; calc-copy-as-kill
      ;; calc-copy-to-buffer
      ;; calc-copy-to-register
      ;; calc-copy-variable
      ;; calc-declare-variable
      ;; calc-insert-register
      ;; calc-insert-variables
      ;; calc-permanent-variable
      ;; calc-recall
      ;; calc-recall-quick
      ;; calc-store
      ;; calc-store-concat
      ;; calc-store-decr
      ;; calc-store-div
      ;; calc-store-exchange
      ;; calc-store-incr
      ;; calc-store-into
      ;; calc-store-into-quick
      ;; calc-store-inv
      ;; calc-store-map
      ;; calc-store-neg
      ;; calc-store-power
      ;; calc-store-quick
      ;; calc-store-times
      ;; calc-unstore
      ;; calc-user-define
      ;; calc-user-define-composition
      ;; calc-user-define-edit
      ;; calc-user-define-formula
      ;; calc-user-define-invocation
      ;; calc-user-define-permanent
      ;; calc-user-undefine
      ;; calc-get-user-defn
      )

(map! :map jg-calc-mode-map ;; select
      :prefix ("v" . "select")
      ;; calc-copy-selection
      ;; calc-break-selections
      ;; calc-clear-selections
      ;; calc-del-selection
      ;; calc-edit-selection
      ;; calc-enable-selections
      ;; calc-enter-selection
      ;; calc-select-additional
      ;; calc-select-here
      ;; calc-select-here-maybe
      ;; calc-select-less
      ;; calc-select-more
      ;; calc-select-next
      ;; calc-select-once
      ;; calc-select-once-maybe
      ;; calc-select-part
      ;; calc-select-previous
      ;; calc-show-selections
      ;; calc-unselect
      )

(map! :map jg-calc-mode-map ;; vectors
      :prefix ("V" . "Vectors")
      :n "T"  'calc-transpose
      :n "t"        'calc-transpose-lines
      ;; calc-begin-vector
      ;; calc-end-vectr
      ;; calc-grade
      ;; calc-head
      ;; calc-index
      ;; calc-cons
      ;; calc-pack
      ;; calc-unpack
      ;; calc-[begin, end]-vector
      ;; calc-arrange-vector
      ;; calc-break-vectors
      ;; calc-build-vector
      ;; calc-diag
      ;; calc-expand-vector
      ;; calc-mask-vector
      ;; calc-mcol
      ;; calc-mrow
      ;; calc-subscript
      ;; calc-reverse-vector
      ;; calc-subvector
      ;; calc-vector-find
      ;; calc-vector-rms
      ;; calc-vector-sdev
      ;; calc-vector-sum
      ;; calc-vlength
      )

(map! :map jg-calc-mode-map ;; local
      :localleader
      (:prefix ("m" . "macros")
               ;; calc-call-last-kbd-macro
               ;; calc-user-define-kbd-macro
               )
      (:prefix ("s" . "settings")
               ;; calc-working
               ;; calc-auto-recompute
               ;; calc-timing
               ;; calc-edit
               ;; calc-edit-AlgSimpRules
               ;; calc-edit-Decls
               ;; calc-edit-ExtSimpRules
               ;; calc-edit-FitRules
               ;; calc-edit-GenCount
               ;; calc-edit-Holidays
               ;; calc-edit-IntegLimit
               ;; calc-edit-LineStyles
               ;; calc-edit-PlotRejects
               ;; calc-edit-PointStyles
               ;; calc-edit-TimeZone
               ;; calc-edit-user-syntax
               ;; calc-edit-variable
               ;; calc-kbd-break
               ;; calc-kbd-else
               ;; calc-kbd-else-if
               ;; calc-kbd-end-for
               ;; calc-kbd-end-if
               ;; calc-kbd-end-loop
               ;; calc-kbd-end-repeat
               ;; calc-kbd-for
               ;; calc-kbd-if
               ;; calc-kbd-loop
               ;; calc-kbd-pop
               ;; calc-kbd-push
               ;; calc-kbd-query
               ;; calc-kbd-repeat
               ;; calc-load-everything
               ;; calc-more-recursion-depth
               ;; calc-settings-file-name
               )
      (:prefix ("g" . "Graph")
               ;; calc-graph-add
               ;; calc-graph-add-3d
               ;; calc-graph-border
               ;; calc-graph-clear
               ;; calc-graph-delete
               ;; calc-graph-device
               ;; calc-graph-display
               ;; calc-graph-fast
               ;; calc-graph-fast-3d
               ;; calc-graph-geometry
               ;; calc-graph-grid
               ;; calc-graph-header
               ;; calc-graph-hide
               ;; calc-graph-juggle
               ;; calc-graph-key
               ;; calc-graph-kill
               ;; calc-graph-line-style
               ;; calc-graph-log-x
               ;; calc-graph-log-y
               ;; calc-graph-log-z
               ;; calc-graph-name
               ;; calc-graph-num-points
               ;; calc-graph-output
               ;; calc-graph-plot
               ;; calc-graph-point-style
               ;; calc-graph-print
               ;; calc-graph-quit
               ;; calc-graph-range-x
               ;; calc-graph-range-y
               ;; calc-graph-range-z
               ;; calc-graph-title-x
               ;; calc-graph-title-y
               ;; calc-graph-title-z
               ;; calc-graph-view-trail
               ;; calc-graph-zero-x
               ;; calc-graph-zero-y
               ;; calc-histogram
                     )
      (:prefix ("d" . "Display")

       ;; calc-float
       ;; calc-fraction
       :n "<"  'calc-scroll-left
       :n ">"  'calc-scroll-right
       :n "s" 'calc-display-strings

       ;; calc-full-vectors
       ;; calc-[left, right] label
       ;; calc-auto-why
       ;; calc-center-justify
       ;; calc-display-raw
       ;; calc-group-char calc-group-digits
       ;; calc-leading-zeros
       ;; calc-left-justify
       ;; calc-line-[breaking, numbering]
       ;; calc-matrix-[right, center, left]-justify
       ;; calc-matrix-brackets
       ;; calc-point-char
       ;; calc-refresh
       ;; calc-refresh-top
       ;; calc-right-justify
       ;; calc-show-plain
       ;; calc-toggle-banner
       ;; calc-truncate-[up, down, stack]
       ;; calc-vector-braces
       ;; calc-vector-brackets
       ;; calc-vector-parens
       ;; calc-why
       )
      (:prefix ("t" . "trail")
               ;; calc-trail-backward
               ;; calc-trail-display
               ;; calc-trail-display
               ;; calc-trail-first
               ;; calc-trail-forward
               ;; calc-trail-here
               ;; calc-trail-in
               ;; calc-trail-isearch-backward
               ;; calc-trail-isearch-forward
               ;; calc-trail-kill
               ;; calc-trail-last
               ;; calc-trail-marker
               ;; calc-trail-next
               ;; calc-trail-out
               ;; calc-trail-previous
               ;; calc-trail-scroll-left
               ;; calc-trail-scroll-right
               ;; calc-trail-yank
               ;; calc-full-trail-vectors
               )
      )

(map! :map jg-calc-dispatch-map ;; dispatch
      ;; #        calc-same-interface

      (:prefix ("e" . "Embedded")
               ;; calc-embedded
               ;; calc-embedded
               ;; calc-embedded-activate
               ;; calc-embedded-duplicate
               ;; calc-embedded-edit
               ;; calc-embedded-new-formula
               ;; calc-embedded-next
               ;; calc-embedded-previous
               ;; calc-embedded-select
               ;; calc-embedded-update-formula
               ;; calc-embedded-word
               )

      ;; calc
      ;; calc-big-or-small
      ;; calc-copy-to-buffer
      ;; calc-dispatch-help
      ;; calc-grab-rectangle
      ;; calc-grab-region
      ;; calc-grab-sum-across
      ;; calc-grab-sum-down
      ;; calc-info
      ;; calc-info-summary
      ;; calc-keypad
      ;; calc-load-everything
      ;; calc-other-window
      ;; calc-quit
      ;; calc-reset
      ;; calc-tutorial
      ;; calc-user-invocation
      )

;; (setq calc-mode-map jg-calc-mode-map)


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 31, 2024
;; Modified:   January 31, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; calc-bindings.el ends here
