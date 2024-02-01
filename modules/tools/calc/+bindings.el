;;; calc-bindings.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defvar jg-calc-mode-map (make-sparse-keymap))

(defvar jg-calc-dispatch-map (make-sparse-keymap))

(map! :map jg-calc-mode-map ;; basic
      :n "!"        'calc-factorial
      :n "+"        'calc-plus
      :n "-"        'calc-minus
      :n "*"        'calc-times
      :n "/"        'calc-divide
      :n ":"        'calc-fdiv
      :n ";"        'calc-semi
      :n "%"        'calc-mod
      :n "&"        'calc-inv
      :n "^"        'calc-power
      :n "%"        'calc-percent
      ;; calc-percent-change
      ;; calc-convert-percent

      ;; TODO evil-calc-complex-state
      :n "("        'calc-begin-complex
      :n ")"        'calc-end-complex
      :n "," 'calc-comma

      :n "RET"      'calc-enter
      ;; calc-enter-selection
      :n "="        'calc-evaluate
      ;; calc-eval-num
      ;; calc-evalto
      ;; calc-edit-EvalRules
      :n "DEL"      'calc-pop
      :n "DEL" 'calc-pop-above
      :n "?"        'calc-help

      ;; :n "Y"        'calc-shift-Y-prefix-help
      :n "o"        'calc-over
      :n "k"        'calc-roll-up
      :n "j"        'calc-roll-down
      :n "l"        'calc-last-args-stub
      :n "-"        'calc-change-sign
      :n "q"        'calc-quit
      :n "u"        'calc-undo
      :n "U"        'calc-redo
      :n "y"        'calc-yank

      ;; calc-copy-special-constant
      ;; calc-remove-duplicates
      :n "~" 'calc-num-prefix
      )

(map! :map jg-calc-mode-map ;; numbers
      :n "£" 'calcDigit-start
      :n "e" 'calcDigit-start
      :n "0" 'calcDigit-start
      :n "1" 'calcDigit-start
      :n "2" 'calcDigit-start
      :n "3" 'calcDigit-start
      :n "4" 'calcDigit-start
      :n "5" 'calcDigit-start
      :n "6" 'calcDigit-start
      :n "7" 'calcDigit-start
      :n "8" 'calcDigit-start
      :n "9" 'calcDigit-start
      :n "." 'calcDigit-start
      :n "#" 'calcDigit-start
      :n "@" 'calcDigit-start
      :n "p" 'calc-precision
      )

(map! :map jg-calc-mode-map ;; modes
      :prefix ("m" . "modes")
      ;; calc-alg-simplify-mode
      ;; calc-algebraic-mode
      ;; calc-basic-simplify-mode
      ;; calc-bin-simplify-mode
      ;; calc-default-simplify-mode
      ;; calc-degrees-mode
      ;; calc-embedded-preserve-modes
      ;; calc-ext-simplify-mode
      ;; calc-frac-mode
      ;; calc-get-modes
      ;; calc-hms-mode
      ;; calc-infinite-mode
      ;; calc-matrix-mode
      ;; calc-no-simplify-mode
      ;; calc-num-simplify-mode
      ;; calc-polar-mode
      ;; calc-radians-mode
      ;; calc-save-modes
      ;; calc-symbolic-mode
      ;; calc-total-algebraic-mode
      ;; calc-units-simplify-mode
      )

(map! :map jg-calc-mode-map ;; settings
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
      ;; calc-edit-selection
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
      ;; calc-option
      ;; calc-settings-file-name
      )

(map! :map jg-calc-map ;; algebra
      :prefix ("A" . "Algebra")
      "\"" 'calc-auto-algebraic-entry
      "$"  'calc-auto-algebraic-entry
      "'"  'calc-algebraic-entry
      ;; calc-argument
      ;; calc-derivative
      ;; calc-expand
      ;; calc-expand-formula
      ;; calc-expm1
      ;; calc-fin-sln
      ;; calc-histogram
      ;; calc-integral
      ;; calc-keep-args
      ;; calc-last-args
      ;; calc-lnp1
      ;; calc-num-integral
      ;; calc-rewrite
      ;; calc-rewrite-selection
      ;; calc-sel-add-both-sides
      ;; calc-sel-commute
      ;; calc-sel-distribute
      ;; calc-sel-div-both-sides
      ;; calc-sel-expand-formula
      ;; calc-sel-invert
      ;; calc-sel-isolate
      ;; calc-sel-jump-equals
      ;; calc-sel-merge
      ;; calc-sel-mult-both-sides
      ;; calc-sel-negate
      ;; calc-sel-sub-both-sides
      ;; calc-sel-unpack
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
      ;; calc-simplify
      ;; calc-simplify-extended
      ;; calc-solve-for
      ;; calc-summation
      )

(map! :map jg-calc-map ;; graphs
      :prefix ("g" . "Graph")
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

(map! :map jg-calc-mode-map ;; vectors
      :prefix ("v" . "Vectors")
      :n "T"  'calc-transpose
      :n "t"        'calc-transpose-lines
      ;; calc-reduce
      ;; calc-pack
      ;; calc-accumulate
      ;; calc-unpack
      ;; calc-[begin, end]-vector
      ;; calc-[outer, inner]-product
      ;; calc-arrange-vector
      ;; calc-break-vectors
      ;; calc-build-vector
      ;; calc-convert-temperature
      ;; calc-cross
      ;; calc-diag
      ;; calc-expand-vector
      ;; calc-full-trail-vectors
      ;; calc-full-vectors
      ;; calc-map
      ;; calc-map-equation
      ;; calc-mask-vector
      ;; calc-mcol
      ;; calc-mdet
      ;; calc-mlud
      ;; calc-mrow
      ;; calc-mtrace
      ;; calc-product
      ;; calc-reverse-vector
      ;; calc-subvector
      ;; calc-vector-count
      ;; calc-vector-count
      ;; calc-vector-covariance
      ;; calc-vector-covariance
      ;; calc-vector-find
      ;; calc-vector-geometric-mean
      ;; calc-vector-geometric-mean
      ;; calc-vector-max
      ;; calc-vector-max
      ;; calc-vector-mean
      ;; calc-vector-mean
      ;; calc-vector-min
      ;; calc-vector-min
      ;; calc-vector-product
      ;; calc-vector-product
      ;; calc-vector-rms
      ;; calc-vector-rms
      ;; calc-vector-sdev
      ;; calc-vector-sdev
      ;; calc-vector-sum
      ;; calc-vector-sum
      )

(map! :map jg-calc-mode-map ;; display
      :prefix ("d" . "Display")

      ;; calc-float
      ;; calc-fraction
      :n "<"  'calc-scroll-left
      :n ">"  'calc-scroll-right
      :n "s" 'calc-display-strings

      ;; calc-[complex, date, eng, fix, hms, i, j, normal, over, sci]-notation
      ;; calc-[left, right] label
      ;; calc-auto-recompute
      ;; calc-auto-why
      ;; calc-binary-radix
      ;; calc-center-justify
      ;; calc-conj-transpose
      ;; calc-decimal-radix
      ;; calc-display-raw
      ;; calc-graph-display
      ;; calc-graph-line-style
      ;; calc-group-char calc-group-digits
      ;; calc-hex-radix
      ;; calc-leading-zeros
      ;; calc-left-justify
      ;; calc-line-[breaking, numbering]
      ;; calc-matrix-[right, center, left]-justify
      ;; calc-matrix-brackets
      ;; calc-octal-radix
      ;; calc-point-char
      ;; calc-radix
      ;; calc-refresh
      ;; calc-refresh-top
      ;; calc-right-justify
      ;; calc-set-cardinality
      ;; calc-show-[plain, selections]
      ;; calc-toggle-banner
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
      ;; calc-truncate-[up, down, stack]
      ;; calc-vector-braces
      ;; calc-vector-brackets
      ;; calc-vector-parens
      ;; calc-why
      )

(map! :map jg-calc-mode-map ;;language
      :prefix ("L" . "Language")
      ;; calc-[giac, big, c, eqn, fortran, latex, mathematic, normal, flat, pascal, unformatted, maple, maxima, yacas, ]-language
      )

(map! :map jg-calc-mode-map ;; units
      :prefix ("u" . "Units")
      ;; calc-autorange-units
      ;; calc-base-units
      ;; calc-convert-exact-units
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
      ;; calc-get-unit-definition
      ;; calc-inc-beta
      ;; calc-inc-gamma
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
      ;; calc-scale-float
      ;; calc-simplify-units
      ;; calc-time-zone
      ;; calc-to-degrees
      ;; calc-to-hms
      ;; calc-to-radians
      ;; calc-totient
      ;; calc-undefine-unit
      ;; calc-unix-time
      ;; calc-view-units-table
      ;; calc-xpon-part
      )

(map! :map jg-calc-mode-map ;; functions
      :prefix ("f" . "funcs")
      :n "A" 'calc-abs
      :n "B" 'calc-log
      :n "C" 'calc-cos
      ;; calc-[pi, sqrt, round, sin, arctan2, isqrt]
      ;; calc-abssqr
      ;; calc-alt-summation
      ;; calc-bernoulli-number
      ;; calc-bessel-J
      ;; calc-bessel-Y
      ;; calc-beta
      ;; calc-change-sign
      ;; calc-choose
      ;; calc-clip
      ;; calc-cnorm
      ;; calc-collect
      ;; calc-commute-left
      ;; calc-commute-right
      ;; calc-concat
      ;; calc-conj
      ;; calc-curve-fit
      ;; calc-erf
      ;; calc-euler-number
      ;; calc-exp
      ;; calc-extended-gcd
      ;; calc-factor
      ;; calc-find-maximum
      ;; calc-find-minimum
      ;; calc-find-root
      ;; calc-floor
      ;; calc-freq
      ;; calc-gamma
      ;; calc-gcd
      ;; calc-hyperbolic
      ;; calc-hypot
      ;; calc-ident
      ;; calc-idiv
      ;; calc-ilog
      ;; calc-im
      ;; calc-ln
      ;; calc-log
      ;; calc-max
      ;; calc-min
      ;; calc-moebius
      ;; calc-next-prime
      ;; calc-poly-div
      ;; calc-poly-div-rem
      ;; calc-poly-gcd
      ;; calc-poly-interp
      ;; calc-poly-rem
      ;; calc-poly-roots
      ;; calc-power
      ;; calc-prime-factors
      ;; calc-prime-test
      ;; calc-random
      ;; calc-random-again
      ;; calc-re
      ;; calc-set-floor
      ;; calc-shuffle
      ;; calc-sign
      ;; calc-stirling-number
      ;; calc-tan
      ;; calc-taylor
      ;; calc-utpb
      ;; calc-utpc
      ;; calc-utpf
      ;; calc-utpn
      ;; calc-utpp
      ;; calc-utpt
      )

(map! :map jg-calc-mode-map ;;logic / binary
      :prefix ("B" . "binary")
      ;; calc-[and, xor, and, or]
      ;; calc-equal-to
      ;; calc-greater-equal
      ;; calc-greater-than
      ;; calc-in-set
      ;; calc-less-equal
      ;; calc-less-than
      ;; calc-logical-and
      ;; calc-logical-if
      ;; calc-logical-not
      ;; calc-logical-or
      ;; calc-lshift-arith
      ;; calc-lshift-binary
      ;; calc-not
      ;; calc-not-equal-to
      ;; calc-pack-bits
      ;; calc-rotate-binary
      ;; calc-rshift-arith
      ;; calc-rshift-binary
      ;; calc-set-cardinality
      ;; calc-set-complement
      ;; calc-set-difference
      ;; calc-set-enumerate
      ;; calc-set-intersect
      ;; calc-set-span
      ;; calc-set-union
      ;; calc-set-xor
      ;; calc-shift-prefix
      ;; calc-unpack-bits
      ;; calc-word-size
      )

(map! :map jg-calc-mode-map ;; state
      :prefix  ("s" . "State")
      ;; calc-copy-as-kill
      ;; calc-copy-selection
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
      ;; calc-user-define-kbd-macro
      ;; calc-user-define-permanent
      ;; calc-user-undefine
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

;;-- unknown
;; calc-apart
;; calc-apply
;; calc-assign
;; calc-break-selections
;; calc-call-last-kbd-macro
;; calc-clean
;; calc-clean-num
;; calc-clear-selections
;; calc-cons
;; calc-db
;; calc-decrement
;; calc-del-selection
;; calc-diff
;; calc-double-factorial
;; calc-enable-selections
;; calc-fin-ddb
;; calc-fin-fv
;; calc-fin-irr
;; calc-fin-nper
;; calc-fin-npv
;; calc-fin-pmt
;; calc-fin-pv
;; calc-fin-rate
;; calc-fin-syd
;; calc-get-user-defn
;; calc-grade
;; calc-head
;; calc-increment
;; calc-index
;; calc-kill
;; calc-kill-region
;; calc-kron
;; calc-lcm
;; calc-let
;; calc-lu-quant
;; calc-lu-times
;; calc-match
;; calc-normalize-rat
;; calc-np
;; calc-remove-duplicates
;; calc-remove-equal
;; calc-rnorm
;; calc-sort
;; calc-spn
;; calc-subscript
;; calc-substitute
;; calc-tabulate
;; calc-timing
;; calc-unselect
;; calc-vlength
;; calc-working

;;-- end unknown

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
