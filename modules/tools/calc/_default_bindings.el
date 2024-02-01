;;; _default_bindings.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;-- basic
;; !                 calc-factorial
;; "                 calc-auto-algebraic-entry
;; #                 calcDigit-start
;; $                 calc-auto-algebraic-entry
;; %                 calc-mod
;; &                 calc-inv
;; '                 calc-algebraic-entry
;; (                 calc-begin-complex
;; )                 calc-end-complex
;; *                 calc-times
;; +                 calc-plus
;; ,                 calc-comma
;; -                 calc-minus
;; .                 calcDigit-start
;; /                 calc-divide
;; :                 calc-fdiv
;; ;                 calc-semi
;; <                 calc-scroll-left
;; <deletechar>      calc-pop
;; <mouse-2>         calc-yank
;; =                 calc-evaluate
;; >                 calc-scroll-right
;; ?                 calc-help
;; @                 calcDigit-start
;; X                 calc-call-last-kbd-macro
;; Y ?               calc-shift-Y-prefix-help
;; Y M-?             calc-shift-Y-prefix-help
;; [                 calc-begin-vector
;; \                 calc-idiv
;; ]                 calc-end-vector
;; ^                 calc-power
;; _                 calcDigit-start
;; `                 calc-edit
;; e                 calcDigit-start
;; h                 calc-help-prefix
;; i                 calc-info
;; n                 calc-change-sign
;; o                 calc-realign
;; p                 calc-precision
;; q                 calc-quit
;; w                 calc-why
;; x                 calc-execute-extended-command
;; y                 calc-copy-to-buffer
;; z ?               calc-z-prefix-help
;; z M-?             calc-z-prefix-help
;; {                 calc-scroll-down
;; |                 calc-concat
;; }                 calc-scroll-up
;; ~                 calc-num-prefix
;;-- end basic

;;-- funcs
;; A                 calc-abs
;; B                 calc-log
;; C                 calc-cos
;; E                 calc-exp
;; F                 calc-floor
;; G                 calc-argument
;; H                 calc-hyperbolic
;; I                 calc-inverse
;; J                 calc-conj
;; K                 calc-keep-args
;; L                 calc-ln
;; N                 calc-eval-num
;; O                 calc-option
;; P                 calc-pi
;; Q                 calc-sqrt
;; R                 calc-round
;; S                 calc-sin
;; T                 calc-tan
;;-- end funcs

;;-- editing
;; C-M-d             calc-pop-above
;; C-M-i             calc-roll-up
;; C-M-w             kill-ring-save
;; C-d               calc-pop
;; C-j               calc-over
;; C-k               calc-kill
;; C-w               calc-kill-region
;; C-x C-t           calc-transpose-lines
;; C-x u             calc-undo
;; C-y               calc-yank
;; D                 calc-redo
;; DEL               calc-pop
;; ESC <deletechar>  calc-pop-above
;; RET               calc-enter
;; SPC               calc-enter
;; TAB               calc-roll-down

;; M                 calc-more-recursion-depth
;; M-%               calc-percent
;; M-DEL             calc-pop-above
;; M-RET             calc-last-args
;; M-k               calc-copy-as-kill
;; M-m M-t           calc-total-algebraic-mode
;; M-m t             calc-total-algebraic-mode
;; M-w               calc-copy-region-as-kill

;; U                 calc-undo

;;-- end editing

;;-- vector
;; V #               calc-set-cardinality
;; V &               calc-inv
;; V (               calc-vector-parens
;; V )               calc-matrix-brackets
;; V +               calc-remove-duplicates
;; V ,               calc-vector-commas
;; V -               calc-set-difference
;; V .               calc-full-vectors
;; V /               calc-break-vectors
;; V :               calc-set-span
;; V <               calc-matrix-left-justify
;; V =               calc-matrix-center-justify
;; V >               calc-matrix-right-justify
;; V ?               calc-v-prefix-help
;; V A               calc-apply
;; V C               calc-cross
;; V D               calc-mdet
;; V E               calc-set-enumerate
;; V F               calc-set-floor
;; V G               calc-grade
;; V H               calc-histogram
;; V I               calc-inner-product
;; V J               calc-conj-transpose
;; V K               calc-kron
;; V L               calc-mlud
;; V M               calc-map
;; V M-#             calc-set-cardinality
;; V M-&             calc-inv
;; V M-(             calc-vector-parens
;; V M-)             calc-matrix-brackets
;; V M-+             calc-remove-duplicates
;; V M-,             calc-vector-commas
;; V M--             calc-set-difference
;; V M-.             calc-full-vectors
;; V M-/             calc-break-vectors
;; V M-:             calc-set-span
;; V M-<             calc-matrix-left-justify
;; V M-=             calc-matrix-center-justify
;; V M->             calc-matrix-right-justify
;; V M-?             calc-v-prefix-help
;; V M-A             calc-apply
;; V M-C             calc-cross
;; V M-D             calc-mdet
;; V M-E             calc-set-enumerate
;; V M-F             calc-set-floor
;; V M-G             calc-grade
;; V M-H             calc-histogram
;; V M-I             calc-inner-product
;; V M-J             calc-conj-transpose
;; V M-K             calc-kron
;; V M-L             calc-mlud
;; V M-M             calc-map
;; V M-N             calc-cnorm
;; V M-O             calc-outer-product
;; V M-R             calc-reduce
;; V M-S             calc-sort
;; V M-T             calc-mtrace
;; V M-U             calc-accumulate
;; V M-V             calc-set-union
;; V M-X             calc-set-xor
;; V M-[             calc-vector-brackets
;; V M-]             calc-matrix-brackets
;; V M-^             calc-set-intersect
;; V M-a             calc-arrange-vector
;; V M-b             calc-build-vector
;; V M-c             calc-mcol
;; V M-d             calc-diag
;; V M-e             calc-expand-vector
;; V M-f             calc-vector-find
;; V M-h             calc-head
;; V M-i             calc-ident
;; V M-k             calc-cons
;; V M-l             calc-vlength
;; V M-m             calc-mask-vector
;; V M-n             calc-rnorm
;; V M-p             calc-pack
;; V M-r             calc-mrow
;; V M-s             calc-subvector
;; V M-t             calc-transpose
;; V M-u             calc-unpack
;; V M-v             calc-reverse-vector
;; V M-x             calc-index
;; V M-{             calc-vector-braces
;; V M-}             calc-matrix-brackets
;; V M-~             calc-set-complement
;; V N               calc-cnorm
;; V O               calc-outer-product
;; V R               calc-reduce
;; V S               calc-sort
;; V T               calc-mtrace
;; V U               calc-accumulate
;; V V               calc-set-union
;; V X               calc-set-xor
;; V [               calc-vector-brackets
;; V ]               calc-matrix-brackets
;; V ^               calc-set-intersect
;; V a               calc-arrange-vector
;; V b               calc-build-vector
;; V c               calc-mcol
;; V d               calc-diag
;; V e               calc-expand-vector
;; V f               calc-vector-find
;; V h               calc-head
;; V i               calc-ident
;; V k               calc-cons
;; V l               calc-vlength
;; V m               calc-mask-vector
;; V n               calc-rnorm
;; V p               calc-pack
;; V r               calc-mrow
;; V s               calc-subvector
;; V t               calc-transpose
;; V u               calc-unpack
;; V v               calc-reverse-vector
;; V x               calc-index
;; V {               calc-vector-braces
;; V }               calc-matrix-brackets
;; V ~               calc-set-complement

;;-- end vector

;;-- user
;; Z #               calc-kbd-query
;; Z '               calc-kbd-pop
;; Z (               calc-kbd-for
;; Z )               calc-kbd-end-for
;; Z /               calc-kbd-break
;; Z :               calc-kbd-else
;; Z <               calc-kbd-repeat
;; Z >               calc-kbd-end-repeat
;; Z ?               calc-shift-Z-prefix-help
;; Z C               calc-user-define-composition
;; Z D               calc-user-define
;; Z E               calc-user-define-edit
;; Z F               calc-user-define-formula
;; Z G               calc-get-user-defn
;; Z I               calc-user-define-invocation
;; Z K               calc-user-define-kbd-macro
;; Z M-#             calc-kbd-query
;; Z M-'             calc-kbd-pop
;; Z M-(             calc-kbd-for
;; Z M-)             calc-kbd-end-for
;; Z M-/             calc-kbd-break
;; Z M-:             calc-kbd-else
;; Z M-<             calc-kbd-repeat
;; Z M->             calc-kbd-end-repeat
;; Z M-?             calc-shift-Z-prefix-help
;; Z M-C             calc-user-define-composition
;; Z M-D             calc-user-define
;; Z M-E             calc-user-define-edit
;; Z M-F             calc-user-define-formula
;; Z M-G             calc-get-user-defn
;; Z M-I             calc-user-define-invocation
;; Z M-K             calc-user-define-kbd-macro
;; Z M-P             calc-user-define-permanent
;; Z M-S             calc-edit-user-syntax
;; Z M-T             calc-timing
;; Z M-U             calc-user-undefine
;; Z M-[             calc-kbd-if
;; Z M-]             calc-kbd-end-if
;; Z M-`             calc-kbd-push
;; Z M-{             calc-kbd-loop
;; Z M-|             calc-kbd-else-if
;; Z M-}             calc-kbd-end-loop
;; Z P               calc-user-define-permanent
;; Z S               calc-edit-user-syntax
;; Z T               calc-timing
;; Z U               calc-user-undefine
;; Z [               calc-kbd-if
;; Z ]               calc-kbd-end-if
;; Z `               calc-kbd-push
;; Z {               calc-kbd-loop
;; Z |               calc-kbd-else-if
;; Z }               calc-kbd-end-loop

;;-- end user

;;-- algebra
;; a !               calc-logical-not
;; a "               calc-expand-formula
;; a #               calc-not-equal-to
;; a %               calc-poly-rem
;; a &               calc-logical-and
;; a *               calc-product
;; a +               calc-summation
;; a -               calc-alt-summation
;; a .               calc-remove-equal
;; a /               calc-poly-div-rem
;; a :               calc-logical-if
;; a <               calc-less-than
;; a =               calc-equal-to
;; a >               calc-greater-than
;; a ?               calc-a-prefix-help
;; a A               calc-abs
;; a F               calc-curve-fit
;; a I               calc-num-integral
;; a M               calc-map-equation
;; a M-!             calc-logical-not
;; a M-"             calc-expand-formula
;; a M-#             calc-not-equal-to
;; a M-%             calc-poly-rem
;; a M-&             calc-logical-and
;; a M-*             calc-product
;; a M-+             calc-summation
;; a M--             calc-alt-summation
;; a M-.             calc-remove-equal
;; a M-/             calc-poly-div-rem
;; a M-:             calc-logical-if
;; a M-<             calc-less-than
;; a M-=             calc-equal-to
;; a M->             calc-greater-than
;; a M-?             calc-a-prefix-help
;; a M-A             calc-abs
;; a M-F             calc-curve-fit
;; a M-I             calc-num-integral
;; a M-M             calc-map-equation
;; a M-N             calc-find-minimum
;; a M-P             calc-poly-roots
;; a M-R             calc-find-root
;; a M-S             calc-solve-for
;; a M-T             calc-tabulate
;; a M-X             calc-find-maximum
;; a M-[             calc-less-equal
;; a M-\             calc-poly-div
;; a M-]             calc-greater-equal
;; a M-_             calc-subscript
;; a M-a             calc-apart
;; a M-b             calc-substitute
;; a M-c             calc-collect
;; a M-d             calc-derivative
;; a M-e             calc-simplify-extended
;; a M-f             calc-factor
;; a M-g             calc-poly-gcd
;; a M-i             calc-integral
;; a M-m             calc-match
;; a M-n             calc-normalize-rat
;; a M-p             calc-poly-interp
;; a M-r             calc-rewrite
;; a M-s             calc-simplify
;; a M-t             calc-taylor
;; a M-v             calc-alg-evaluate
;; a M-x             calc-expand
;; a M-{             calc-in-set
;; a M-|             calc-logical-or
;; a N               calc-find-minimum
;; a P               calc-poly-roots
;; a R               calc-find-root
;; a S               calc-solve-for
;; a T               calc-tabulate
;; a X               calc-find-maximum
;; a [               calc-less-equal
;; a \               calc-poly-div
;; a ]               calc-greater-equal
;; a _               calc-subscript
;; a a               calc-apart
;; a b               calc-substitute
;; a c               calc-collect
;; a d               calc-derivative
;; a e               calc-simplify-extended
;; a f               calc-factor
;; a g               calc-poly-gcd
;; a i               calc-integral
;; a m               calc-match
;; a n               calc-normalize-rat
;; a p               calc-poly-interp
;; a r               calc-rewrite
;; a s               calc-simplify
;; a t               calc-taylor
;; a v               calc-alg-evaluate
;; a x               calc-expand
;; a {               calc-in-set
;; a |               calc-logical-or

;;-- end algebra

;;-- binary
;; b #               calc-fin-nper
;; b %               calc-percent-change
;; b ?               calc-b-prefix-help
;; b B               calc-log
;; b D               calc-fin-ddb
;; b F               calc-fin-fv
;; b I               calc-fin-irr
;; b L               calc-lshift-arith
;; b M               calc-fin-pmt
;; b M-#             calc-fin-nper
;; b M-%             calc-percent-change
;; b M-?             calc-b-prefix-help
;; b M-B             calc-log
;; b M-D             calc-fin-ddb
;; b M-F             calc-fin-fv
;; b M-I             calc-fin-irr
;; b M-L             calc-lshift-arith
;; b M-M             calc-fin-pmt
;; b M-N             calc-fin-npv
;; b M-P             calc-fin-pv
;; b M-R             calc-rshift-arith
;; b M-S             calc-fin-sln
;; b M-T             calc-fin-rate
;; b M-Y             calc-fin-syd
;; b M-a             calc-and
;; b M-c             calc-clip
;; b M-d             calc-diff
;; b M-l             calc-lshift-binary
;; b M-n             calc-not
;; b M-o             calc-or
;; b M-p             calc-pack-bits
;; b M-r             calc-rshift-binary
;; b M-t             calc-rotate-binary
;; b M-u             calc-unpack-bits
;; b M-w             calc-word-size
;; b M-x             calc-xor
;; b N               calc-fin-npv
;; b P               calc-fin-pv
;; b R               calc-rshift-arith
;; b S               calc-fin-sln
;; b T               calc-fin-rate
;; b Y               calc-fin-syd
;; b a               calc-and
;; b c               calc-clip
;; b d               calc-diff
;; b l               calc-lshift-binary
;; b n               calc-not
;; b o               calc-or
;; b p               calc-pack-bits
;; b r               calc-rshift-binary
;; b t               calc-rotate-binary
;; b u               calc-unpack-bits
;; b w               calc-word-size
;; b x               calc-xor

;;-- end binary

;;-- precision
;; c %               calc-convert-percent
;; c 0               calc-clean-num
;; c 1               calc-clean-num
;; c 2               calc-clean-num
;; c 3               calc-clean-num
;; c 4               calc-clean-num
;; c 5               calc-clean-num
;; c 6               calc-clean-num
;; c 7               calc-clean-num
;; c 8               calc-clean-num
;; c 9               calc-clean-num
;; c ?               calc-c-prefix-help
;; c C               calc-cos
;; c F               calc-fraction
;; c M-%             calc-convert-percent
;; c M-0             calc-clean-num
;; c M-1             calc-clean-num
;; c M-2             calc-clean-num
;; c M-3             calc-clean-num
;; c M-4             calc-clean-num
;; c M-5             calc-clean-num
;; c M-6             calc-clean-num
;; c M-7             calc-clean-num
;; c M-8             calc-clean-num
;; c M-9             calc-clean-num
;; c M-?             calc-c-prefix-help
;; c M-C             calc-cos
;; c M-F             calc-fraction
;; c M-c             calc-clean
;; c M-d             calc-to-degrees
;; c M-f             calc-float
;; c M-h             calc-to-hms
;; c M-p             calc-polar
;; c M-r             calc-to-radians
;; c c               calc-clean
;; c d               calc-to-degrees
;; c f               calc-float
;; c h               calc-to-hms
;; c p               calc-polar
;; c r               calc-to-radians

;;-- end precision

;;-- display
;; d "               calc-display-strings
;; d '               calc-display-raw
;; d ,               calc-group-char
;; d .               calc-point-char
;; d 0               calc-decimal-radix
;; d 2               calc-binary-radix
;; d 6               calc-hex-radix
;; d 8               calc-octal-radix
;; d <               calc-left-justify
;; d =               calc-center-justify
;; d >               calc-right-justify
;; d ?               calc-d-prefix-help
;; d @               calc-toggle-banner
;; d A               calc-giac-language
;; d B               calc-big-language
;; d C               calc-c-language
;; d D               calc-redo
;; d E               calc-eqn-language
;; d F               calc-fortran-language
;; d L               calc-latex-language
;; d M               calc-mathematica-language
;; d M-"             calc-display-strings
;; d M-'             calc-display-raw
;; d M-,             calc-group-char
;; d M-.             calc-point-char
;; d M-0             calc-decimal-radix
;; d M-2             calc-binary-radix
;; d M-6             calc-hex-radix
;; d M-8             calc-octal-radix
;; d M-<             calc-left-justify
;; d M-=             calc-center-justify
;; d M->             calc-right-justify
;; d M-?             calc-d-prefix-help
;; d M-@             calc-toggle-banner
;; d M-A             calc-giac-language
;; d M-B             calc-big-language
;; d M-C             calc-c-language
;; d M-D             calc-redo
;; d M-E             calc-eqn-language
;; d M-F             calc-fortran-language
;; d M-L             calc-latex-language
;; d M-M             calc-mathematica-language
;; d M-N             calc-normal-language
;; d M-O             calc-flat-language
;; d M-P             calc-pascal-language
;; d M-RET           calc-refresh-top
;; d M-SPC           calc-refresh
;; d M-T             calc-tex-language
;; d M-U             calc-unformatted-language
;; d M-W             calc-maple-language
;; d M-X             calc-maxima-language
;; d M-Y             calc-yacas-language
;; d M-[             calc-truncate-up
;; d M-]             calc-truncate-down
;; d M-b             calc-line-breaking
;; d M-c             calc-complex-notation
;; d M-d             calc-date-notation
;; d M-e             calc-eng-notation
;; d M-f             calc-fix-notation
;; d M-g             calc-group-digits
;; d M-h             calc-hms-notation
;; d M-i             calc-i-notation
;; d M-j             calc-j-notation
;; d M-l             calc-line-numbering
;; d M-n             calc-normal-notation
;; d M-o             calc-over-notation
;; d M-p             calc-show-plain
;; d M-r             calc-radix
;; d M-s             calc-sci-notation
;; d M-t             calc-truncate-stack
;; d M-w             calc-auto-why
;; d M-z             calc-leading-zeros
;; d M-{             calc-left-label
;; d M-}             calc-right-label
;; d N               calc-normal-language
;; d O               calc-flat-language
;; d P               calc-pascal-language
;; d RET             calc-refresh-top
;; d SPC             calc-refresh
;; d T               calc-tex-language
;; d U               calc-unformatted-language
;; d W               calc-maple-language
;; d X               calc-maxima-language
;; d Y               calc-yacas-language
;; d [               calc-truncate-up
;; d ]               calc-truncate-down
;; d b               calc-line-breaking
;; d c               calc-complex-notation
;; d d               calc-date-notation
;; d e               calc-eng-notation
;; d f               calc-fix-notation
;; d g               calc-group-digits
;; d h               calc-hms-notation
;; d i               calc-i-notation
;; d j               calc-j-notation
;; d l               calc-line-numbering
;; d n               calc-normal-notation
;; d o               calc-over-notation
;; d p               calc-show-plain
;; d r               calc-radix
;; d s               calc-sci-notation
;; d t               calc-truncate-stack
;; d w               calc-auto-why
;; d z               calc-leading-zeros
;; d {               calc-left-label
;; d }               calc-right-label

;;-- end display

;;-- funcs
;; f ?               calc-f-prefix-help
;; f A               calc-abssqr
;; f B               calc-inc-beta
;; f E               calc-expm1
;; f F               calc-floor
;; f G               calc-inc-gamma
;; f I               calc-ilog
;; f L               calc-lnp1
;; f M               calc-mant-part
;; f M-?             calc-f-prefix-help
;; f M-A             calc-abssqr
;; f M-B             calc-inc-beta
;; f M-E             calc-expm1
;; f M-F             calc-floor
;; f M-G             calc-inc-gamma
;; f M-I             calc-ilog
;; f M-L             calc-lnp1
;; f M-M             calc-mant-part
;; f M-Q             calc-isqrt
;; f M-S             calc-scale-float
;; f M-T             calc-arctan2
;; f M-X             calc-xpon-part
;; f M-[             calc-decrement
;; f M-]             calc-increment
;; f M-b             calc-beta
;; f M-e             calc-erf
;; f M-g             calc-gamma
;; f M-h             calc-hypot
;; f M-i             calc-im
;; f M-j             calc-bessel-J
;; f M-n             calc-min
;; f M-r             calc-re
;; f M-s             calc-sign
;; f M-x             calc-max
;; f M-y             calc-bessel-Y
;; f Q               calc-isqrt
;; f S               calc-scale-float
;; f T               calc-arctan2
;; f X               calc-xpon-part
;; f [               calc-decrement
;; f ]               calc-increment
;; f b               calc-beta
;; f e               calc-erf
;; f g               calc-gamma
;; f h               calc-hypot
;; f i               calc-im
;; f j               calc-bessel-J
;; f n               calc-min
;; f r               calc-re
;; f s               calc-sign
;; f x               calc-max
;; f y               calc-bessel-Y

;;-- end funcs

;;-- graphs
;; g ?               calc-g-prefix-help
;; g A               calc-graph-add-3d
;; g C               calc-graph-command
;; g C-M-l           calc-graph-log-z
;; g C-M-r           calc-graph-range-z
;; g C-M-t           calc-graph-title-z
;; g C-l             calc-graph-log-z
;; g C-r             calc-graph-range-z
;; g C-t             calc-graph-title-z
;; g D               calc-graph-device
;; g F               calc-graph-fast-3d
;; g G               calc-argument
;; g H               calc-graph-hide
;; g K               calc-graph-kill
;; g L               calc-graph-log-y
;; g M-?             calc-g-prefix-help
;; g M-A             calc-graph-add-3d
;; g M-C             calc-graph-command
;; g M-D             calc-graph-device
;; g M-F             calc-graph-fast-3d
;; g M-G             calc-argument
;; g M-H             calc-graph-hide
;; g M-K             calc-graph-kill
;; g M-L             calc-graph-log-y
;; g M-N             calc-graph-num-points
;; g M-O             calc-graph-output
;; g M-P             calc-graph-print
;; g M-R             calc-graph-range-y
;; g M-S             calc-graph-point-style
;; g M-T             calc-graph-title-y
;; g M-V             calc-graph-view-trail
;; g M-X             calc-graph-geometry
;; g M-Z             calc-graph-zero-y
;; g M-a             calc-graph-add
;; g M-b             calc-graph-border
;; g M-c             calc-graph-clear
;; g M-d             calc-graph-delete
;; g M-f             calc-graph-fast
;; g M-g             calc-graph-grid
;; g M-h             calc-graph-header
;; g M-j             calc-graph-juggle
;; g M-k             calc-graph-key
;; g M-l             calc-graph-log-x
;; g M-n             calc-graph-name
;; g M-p             calc-graph-plot
;; g M-q             calc-graph-quit
;; g M-r             calc-graph-range-x
;; g M-s             calc-graph-line-style
;; g M-t             calc-graph-title-x
;; g M-v             calc-graph-view-commands
;; g M-x             calc-graph-display
;; g M-z             calc-graph-zero-x
;; g N               calc-graph-num-points
;; g O               calc-graph-output
;; g P               calc-graph-print
;; g R               calc-graph-range-y
;; g S               calc-graph-point-style
;; g T               calc-graph-title-y
;; g V               calc-graph-view-trail
;; g X               calc-graph-geometry
;; g Z               calc-graph-zero-y
;; g a               calc-graph-add
;; g b               calc-graph-border
;; g c               calc-graph-clear
;; g d               calc-graph-delete
;; g f               calc-graph-fast
;; g g               calc-graph-grid
;; g h               calc-graph-header
;; g j               calc-graph-juggle
;; g k               calc-graph-key
;; g l               calc-graph-log-x
;; g n               calc-graph-name
;; g p               calc-graph-plot
;; g q               calc-graph-quit
;; g r               calc-graph-range-x
;; g s               calc-graph-line-style
;; g t               calc-graph-title-x
;; g v               calc-graph-view-commands
;; g x               calc-graph-display
;; g z               calc-graph-zero-x

;;-- end graphs

;;-- selection
;; j "               calc-sel-expand-formula
;; j &               calc-sel-invert
;; j '               calc-enter-selection
;; j *               calc-sel-mult-both-sides
;; j +               calc-sel-add-both-sides
;; j -               calc-sel-sub-both-sides
;; j /               calc-sel-div-both-sides
;; j 0               calc-select-part
;; j 1               calc-select-part
;; j 2               calc-select-part
;; j 3               calc-select-part
;; j 4               calc-select-part
;; j 5               calc-select-part
;; j 6               calc-select-part
;; j 7               calc-select-part
;; j 8               calc-select-part
;; j 9               calc-select-part
;; j ?               calc-j-prefix-help
;; j C               calc-sel-commute
;; j C-M-h           calc-del-selection
;; j C-M-j           calc-copy-selection
;; j C-h             calc-del-selection
;; j C-j             calc-copy-selection
;; j D               calc-sel-distribute
;; j DEL             calc-del-selection
;; j E               calc-sel-jump-equals
;; j I               calc-sel-isolate
;; j J               calc-conj
;; j L               calc-commute-left
;; j M               calc-sel-merge
;; j M-"             calc-sel-expand-formula
;; j M-&             calc-sel-invert
;; j M-'             calc-enter-selection
;; j M-*             calc-sel-mult-both-sides
;; j M-+             calc-sel-add-both-sides
;; j M--             calc-sel-sub-both-sides
;; j M-/             calc-sel-div-both-sides
;; j M-0             calc-select-part
;; j M-1             calc-select-part
;; j M-2             calc-select-part
;; j M-3             calc-select-part
;; j M-4             calc-select-part
;; j M-5             calc-select-part
;; j M-6             calc-select-part
;; j M-7             calc-select-part
;; j M-8             calc-select-part
;; j M-9             calc-select-part
;; j M-?             calc-j-prefix-help
;; j M-C             calc-sel-commute
;; j M-D             calc-sel-distribute
;; j M-DEL           calc-del-selection
;; j M-E             calc-sel-jump-equals
;; j M-I             calc-sel-isolate
;; j M-J             calc-conj
;; j M-L             calc-commute-left
;; j M-M             calc-sel-merge
;; j M-N             calc-sel-negate
;; j M-O             calc-select-once-maybe
;; j M-R             calc-commute-right
;; j M-RET           calc-copy-selection
;; j M-S             calc-select-here-maybe
;; j M-U             calc-sel-unpack
;; j M-`             calc-edit-selection
;; j M-a             calc-select-additional
;; j M-b             calc-break-selections
;; j M-c             calc-clear-selections
;; j M-d             calc-show-selections
;; j M-e             calc-enable-selections
;; j M-l             calc-select-less
;; j M-m             calc-select-more
;; j M-n             calc-select-next
;; j M-o             calc-select-once
;; j M-p             calc-select-previous
;; j M-r             calc-rewrite-selection
;; j M-s             calc-select-here
;; j M-u             calc-unselect
;; j M-v             calc-sel-evaluate
;; j N               calc-sel-negate
;; j O               calc-select-once-maybe
;; j R               calc-commute-right
;; j RET             calc-copy-selection
;; j S               calc-select-here-maybe
;; j U               calc-sel-unpack
;; j `               calc-edit-selection
;; j a               calc-select-additional
;; j b               calc-break-selections
;; j c               calc-clear-selections
;; j d               calc-show-selections
;; j e               calc-enable-selections
;; j l               calc-select-less
;; j m               calc-select-more
;; j n               calc-select-next
;; j o               calc-select-once
;; j p               calc-select-previous
;; j r               calc-rewrite-selection
;; j s               calc-select-here
;; j u               calc-unselect
;; j v               calc-sel-evaluate

;;-- end selection

;;-- random
;; k ?               calc-k-prefix-help
;; k B               calc-utpb
;; k C               calc-utpc
;; k E               calc-extended-gcd
;; k F               calc-utpf
;; k K               calc-keep-args
;; k M-?             calc-k-prefix-help
;; k M-B             calc-utpb
;; k M-C             calc-utpc
;; k M-E             calc-extended-gcd
;; k M-F             calc-utpf
;; k M-K             calc-keep-args
;; k M-N             calc-utpn
;; k M-P             calc-utpp
;; k M-T             calc-utpt
;; k M-a             calc-random-again
;; k M-b             calc-bernoulli-number
;; k M-c             calc-choose
;; k M-d             calc-double-factorial
;; k M-e             calc-euler-number
;; k M-f             calc-prime-factors
;; k M-g             calc-gcd
;; k M-h             calc-shuffle
;; k M-l             calc-lcm
;; k M-m             calc-moebius
;; k M-n             calc-next-prime
;; k M-p             calc-prime-test
;; k M-r             calc-random
;; k M-s             calc-stirling-number
;; k M-t             calc-totient
;; k N               calc-utpn
;; k P               calc-utpp
;; k T               calc-utpt
;; k a               calc-random-again
;; k b               calc-bernoulli-number
;; k c               calc-choose
;; k d               calc-double-factorial
;; k e               calc-euler-number
;; k f               calc-prime-factors
;; k g               calc-gcd
;; k h               calc-shuffle
;; k l               calc-lcm
;; k m               calc-moebius
;; k n               calc-next-prime
;; k p               calc-prime-test
;; k r               calc-random
;; k s               calc-stirling-number
;; k t               calc-totient

;;-- end random

;;-- units
;; l *               calc-lu-times
;; l +               calc-lu-plus
;; l -               calc-lu-minus
;; l /               calc-lu-divide
;; l ?               calc-l-prefix-help
;; l M-*             calc-lu-times
;; l M-+             calc-lu-plus
;; l M--             calc-lu-minus
;; l M-/             calc-lu-divide
;; l M-?             calc-l-prefix-help
;; l M-d             calc-db
;; l M-f             calc-freq
;; l M-m             calc-midi
;; l M-n             calc-np
;; l M-q             calc-lu-quant
;; l M-s             calc-spn
;; l d               calc-db
;; l f               calc-freq
;; l m               calc-midi
;; l n               calc-np
;; l q               calc-lu-quant
;; l s               calc-spn

;;-- end units

;;-- modes
;; m ?               calc-m-prefix-help
;; m A               calc-alg-simplify-mode
;; m B               calc-bin-simplify-mode
;; m C               calc-auto-recompute
;; m D               calc-default-simplify-mode
;; m E               calc-ext-simplify-mode
;; m F               calc-settings-file-name
;; m I               calc-basic-simplify-mode
;; m M               calc-more-recursion-depth
;; m M-?             calc-m-prefix-help
;; m M-A             calc-alg-simplify-mode
;; m M-B             calc-bin-simplify-mode
;; m M-C             calc-auto-recompute
;; m M-D             calc-default-simplify-mode
;; m M-E             calc-ext-simplify-mode
;; m M-F             calc-settings-file-name
;; m M-I             calc-basic-simplify-mode
;; m M-M             calc-more-recursion-depth
;; m M-N             calc-num-simplify-mode
;; m M-O             calc-no-simplify-mode
;; m M-R             calc-mode-record-mode
;; m M-S             calc-shift-prefix
;; m M-U             calc-units-simplify-mode
;; m M-X             calc-load-everything
;; m M-a             calc-algebraic-mode
;; m M-d             calc-degrees-mode
;; m M-e             calc-embedded-preserve-modes
;; m M-f             calc-frac-mode
;; m M-g             calc-get-modes
;; m M-h             calc-hms-mode
;; m M-i             calc-infinite-mode
;; m M-m             calc-save-modes
;; m M-p             calc-polar-mode
;; m M-r             calc-radians-mode
;; m M-s             calc-symbolic-mode
;; m M-t             calc-total-algebraic-mode
;; m M-v             calc-matrix-mode
;; m M-w             calc-working
;; m M-x             calc-always-load-extensions
;; m N               calc-num-simplify-mode
;; m O               calc-no-simplify-mode
;; m R               calc-mode-record-mode
;; m S               calc-shift-prefix
;; m U               calc-units-simplify-mode
;; m X               calc-load-everything
;; m a               calc-algebraic-mode
;; m d               calc-degrees-mode
;; m e               calc-embedded-preserve-modes
;; m f               calc-frac-mode
;; m g               calc-get-modes
;; m h               calc-hms-mode
;; m i               calc-infinite-mode
;; m m               calc-save-modes
;; m p               calc-polar-mode
;; m r               calc-radians-mode
;; m s               calc-symbolic-mode
;; m t               calc-total-algebraic-mode
;; m v               calc-matrix-mode
;; m w               calc-working
;; m x               calc-always-load-extensions

;;-- end modes

;;-- recall
;; r 0               calc-recall-quick
;; r 1               calc-recall-quick
;; r 2               calc-recall-quick
;; r 3               calc-recall-quick
;; r 4               calc-recall-quick
;; r 5               calc-recall-quick
;; r 6               calc-recall-quick
;; r 7               calc-recall-quick
;; r 8               calc-recall-quick
;; r 9               calc-recall-quick
;; r ?               calc-r-prefix-help
;; r M-0             calc-recall-quick
;; r M-1             calc-recall-quick
;; r M-2             calc-recall-quick
;; r M-3             calc-recall-quick
;; r M-4             calc-recall-quick
;; r M-5             calc-recall-quick
;; r M-6             calc-recall-quick
;; r M-7             calc-recall-quick
;; r M-8             calc-recall-quick
;; r M-9             calc-recall-quick
;; r M-?             calc-r-prefix-help
;; r M-i             calc-insert-register
;; r M-s             calc-copy-to-register
;; r i               calc-insert-register
;; r s               calc-copy-to-register

;;-- end recall

;;-- store
;; s &               calc-store-inv
;; s *               calc-store-times
;; s +               calc-store-plus
;; s -               calc-store-minus
;; s /               calc-store-div
;; s 0               calc-store-quick
;; s 1               calc-store-quick
;; s 2               calc-store-quick
;; s 3               calc-store-quick
;; s 4               calc-store-quick
;; s 5               calc-store-quick
;; s 6               calc-store-quick
;; s 7               calc-store-quick
;; s 8               calc-store-quick
;; s 9               calc-store-quick
;; s :               calc-assign
;; s =               calc-evalto
;; s ?               calc-s-prefix-help
;; s A               calc-edit-AlgSimpRules
;; s D               calc-edit-Decls
;; s E               calc-edit-EvalRules
;; s F               calc-edit-FitRules
;; s G               calc-edit-GenCount
;; s H               calc-edit-Holidays
;; s I               calc-edit-IntegLimit
;; s L               calc-edit-LineStyles
;; s M-&             calc-store-inv
;; s M-*             calc-store-times
;; s M-+             calc-store-plus
;; s M--             calc-store-minus
;; s M-/             calc-store-div
;; s M-0             calc-store-quick
;; s M-1             calc-store-quick
;; s M-2             calc-store-quick
;; s M-3             calc-store-quick
;; s M-4             calc-store-quick
;; s M-5             calc-store-quick
;; s M-6             calc-store-quick
;; s M-7             calc-store-quick
;; s M-8             calc-store-quick
;; s M-9             calc-store-quick
;; s M-:             calc-assign
;; s M-=             calc-evalto
;; s M-?             calc-s-prefix-help
;; s M-A             calc-edit-AlgSimpRules
;; s M-D             calc-edit-Decls
;; s M-E             calc-edit-EvalRules
;; s M-F             calc-edit-FitRules
;; s M-G             calc-edit-GenCount
;; s M-H             calc-edit-Holidays
;; s M-I             calc-edit-IntegLimit
;; s M-L             calc-edit-LineStyles
;; s M-P             calc-edit-PointStyles
;; s M-R             calc-edit-PlotRejects
;; s M-S             calc-sin
;; s M-T             calc-edit-TimeZone
;; s M-U             calc-edit-Units
;; s M-X             calc-edit-ExtSimpRules
;; s M-[             calc-store-decr
;; s M-]             calc-store-incr
;; s M-^             calc-store-power
;; s M-c             calc-copy-variable
;; s M-d             calc-declare-variable
;; s M-e             calc-edit-variable
;; s M-i             calc-insert-variables
;; s M-k             calc-copy-special-constant
;; s M-l             calc-let
;; s M-m             calc-store-map
;; s M-n             calc-store-neg
;; s M-p             calc-permanent-variable
;; s M-r             calc-recall
;; s M-s             calc-store
;; s M-t             calc-store-into
;; s M-u             calc-unstore
;; s M-x             calc-store-exchange
;; s M-|             calc-store-concat
;; s P               calc-edit-PointStyles
;; s R               calc-edit-PlotRejects
;; s S               calc-sin
;; s T               calc-edit-TimeZone
;; s U               calc-edit-Units
;; s X               calc-edit-ExtSimpRules
;; s [               calc-store-decr
;; s ]               calc-store-incr
;; s ^               calc-store-power
;; s c               calc-copy-variable
;; s d               calc-declare-variable
;; s e               calc-edit-variable
;; s i               calc-insert-variables
;; s k               calc-copy-special-constant
;; s l               calc-let
;; s m               calc-store-map
;; s n               calc-store-neg
;; s p               calc-permanent-variable
;; s r               calc-recall
;; s s               calc-store
;; s t               calc-store-into
;; s u               calc-unstore
;; s x               calc-store-exchange
;; s |               calc-store-concat

;;-- end store

;;-- time/trail
;; t +               calc-business-days-plus
;; t -               calc-business-days-minus
;; t .               calc-full-trail-vectors
;; t 0               calc-store-into-quick
;; t 1               calc-store-into-quick
;; t 2               calc-store-into-quick
;; t 3               calc-store-into-quick
;; t 4               calc-store-into-quick
;; t 5               calc-store-into-quick
;; t 6               calc-store-into-quick
;; t 7               calc-store-into-quick
;; t 8               calc-store-into-quick
;; t 9               calc-store-into-quick
;; t <               calc-trail-scroll-left
;; t >               calc-trail-scroll-right
;; t ?               calc-t-prefix-help
;; t C               calc-convert-time-zones
;; t D               calc-date
;; t I               calc-inc-month
;; t J               calc-julian
;; t M               calc-new-month
;; t M-+             calc-business-days-plus
;; t M--             calc-business-days-minus
;; t M-.             calc-full-trail-vectors
;; t M-0             calc-store-into-quick
;; t M-1             calc-store-into-quick
;; t M-2             calc-store-into-quick
;; t M-3             calc-store-into-quick
;; t M-4             calc-store-into-quick
;; t M-5             calc-store-into-quick
;; t M-6             calc-store-into-quick
;; t M-7             calc-store-into-quick
;; t M-8             calc-store-into-quick
;; t M-9             calc-store-into-quick
;; t M-<             calc-trail-scroll-left
;; t M->             calc-trail-scroll-right
;; t M-?             calc-t-prefix-help
;; t M-C             calc-convert-time-zones
;; t M-D             calc-date
;; t M-I             calc-inc-month
;; t M-J             calc-julian
;; t M-M             calc-new-month
;; t M-N             calc-now
;; t M-P             calc-date-part
;; t M-T             calc-tan
;; t M-U             calc-unix-time
;; t M-W             calc-new-week
;; t M-Y             calc-new-year
;; t M-Z             calc-time-zone
;; t M-[             calc-trail-first
;; t M-]             calc-trail-last
;; t M-b             calc-trail-backward
;; t M-d             calc-trail-display
;; t M-f             calc-trail-forward
;; t M-h             calc-trail-here
;; t M-i             calc-trail-in
;; t M-k             calc-trail-kill
;; t M-m             calc-trail-marker
;; t M-n             calc-trail-next
;; t M-o             calc-trail-out
;; t M-p             calc-trail-previous
;; t M-r             calc-trail-isearch-backward
;; t M-s             calc-trail-isearch-forward
;; t M-y             calc-trail-yank
;; t M-{             calc-trail-backward
;; t M-}             calc-trail-forward
;; t N               calc-now
;; t P               calc-date-part
;; t T               calc-tan
;; t U               calc-unix-time
;; t W               calc-new-week
;; t Y               calc-new-year
;; t Z               calc-time-zone
;; t [               calc-trail-first
;; t ]               calc-trail-last
;; t b               calc-trail-backward
;; t d               calc-trail-display
;; t f               calc-trail-forward
;; t h               calc-trail-here
;; t i               calc-trail-in
;; t k               calc-trail-kill
;; t m               calc-trail-marker
;; t n               calc-trail-next
;; t o               calc-trail-out
;; t p               calc-trail-previous
;; t r               calc-trail-isearch-backward
;; t s               calc-trail-isearch-forward
;; t y               calc-trail-yank
;; t {               calc-trail-backward
;; t }               calc-trail-forward

;;-- end time/trail

;;-- units
;; u #               calc-vector-count
;; u *               calc-vector-product
;; u +               calc-vector-sum
;; u 0               calc-quick-units
;; u 1               calc-quick-units
;; u 2               calc-quick-units
;; u 3               calc-quick-units
;; u 4               calc-quick-units
;; u 5               calc-quick-units
;; u 6               calc-quick-units
;; u 7               calc-quick-units
;; u 8               calc-quick-units
;; u 9               calc-quick-units
;; u ?               calc-u-prefix-help
;; u C               calc-vector-covariance
;; u G               calc-vector-geometric-mean
;; u M               calc-vector-mean
;; u M-#             calc-vector-count
;; u M-*             calc-vector-product
;; u M-+             calc-vector-sum
;; u M-0             calc-quick-units
;; u M-1             calc-quick-units
;; u M-2             calc-quick-units
;; u M-3             calc-quick-units
;; u M-4             calc-quick-units
;; u M-5             calc-quick-units
;; u M-6             calc-quick-units
;; u M-7             calc-quick-units
;; u M-8             calc-quick-units
;; u M-9             calc-quick-units
;; u M-?             calc-u-prefix-help
;; u M-C             calc-vector-covariance
;; u M-G             calc-vector-geometric-mean
;; u M-M             calc-vector-mean
;; u M-N             calc-vector-min
;; u M-R             calc-vector-rms
;; u M-S             calc-vector-sdev
;; u M-U             calc-undo
;; u M-V             calc-view-units-table
;; u M-X             calc-vector-max
;; u M-a             calc-autorange-units
;; u M-b             calc-base-units
;; u M-c             calc-convert-units
;; u M-d             calc-define-unit
;; u M-e             calc-explain-units
;; u M-g             calc-get-unit-definition
;; u M-n             calc-convert-exact-units
;; u M-p             calc-permanent-units
;; u M-r             calc-remove-units
;; u M-s             calc-simplify-units
;; u M-t             calc-convert-temperature
;; u M-u             calc-undefine-unit
;; u M-v             calc-enter-units-table
;; u M-x             calc-extract-units
;; u N               calc-vector-min
;; u R               calc-vector-rms
;; u S               calc-vector-sdev
;; u U               calc-undo
;; u V               calc-view-units-table
;; u X               calc-vector-max
;; u a               calc-autorange-units
;; u b               calc-base-units
;; u c               calc-convert-units
;; u d               calc-define-unit
;; u e               calc-explain-units
;; u g               calc-get-unit-definition
;; u n               calc-convert-exact-units
;; u p               calc-permanent-units
;; u r               calc-remove-units
;; u s               calc-simplify-units
;; u t               calc-convert-temperature
;; u u               calc-undefine-unit
;; u v               calc-enter-units-table
;; u x               calc-extract-units

;;-- end units

;;-- vector
;; v #               calc-set-cardinality
;; v &               calc-inv
;; v (               calc-vector-parens
;; v )               calc-matrix-brackets
;; v +               calc-remove-duplicates
;; v ,               calc-vector-commas
;; v -               calc-set-difference
;; v .               calc-full-vectors
;; v /               calc-break-vectors
;; v :               calc-set-span
;; v <               calc-matrix-left-justify
;; v =               calc-matrix-center-justify
;; v >               calc-matrix-right-justify
;; v ?               calc-v-prefix-help
;; v A               calc-apply
;; v C               calc-cross
;; v D               calc-mdet
;; v E               calc-set-enumerate
;; v F               calc-set-floor
;; v G               calc-grade
;; v H               calc-histogram
;; v I               calc-inner-product
;; v J               calc-conj-transpose
;; v K               calc-kron
;; v L               calc-mlud
;; v M               calc-map
;; v M-#             calc-set-cardinality
;; v M-&             calc-inv
;; v M-(             calc-vector-parens
;; v M-)             calc-matrix-brackets
;; v M-+             calc-remove-duplicates
;; v M-,             calc-vector-commas
;; v M--             calc-set-difference
;; v M-.             calc-full-vectors
;; v M-/             calc-break-vectors
;; v M-:             calc-set-span
;; v M-<             calc-matrix-left-justify
;; v M-=             calc-matrix-center-justify
;; v M->             calc-matrix-right-justify
;; v M-?             calc-v-prefix-help
;; v M-A             calc-apply
;; v M-C             calc-cross
;; v M-D             calc-mdet
;; v M-E             calc-set-enumerate
;; v M-F             calc-set-floor
;; v M-G             calc-grade
;; v M-H             calc-histogram
;; v M-I             calc-inner-product
;; v M-J             calc-conj-transpose
;; v M-K             calc-kron
;; v M-L             calc-mlud
;; v M-M             calc-map
;; v M-N             calc-cnorm
;; v M-O             calc-outer-product
;; v M-R             calc-reduce
;; v M-S             calc-sort
;; v M-T             calc-mtrace
;; v M-U             calc-accumulate
;; v M-V             calc-set-union
;; v M-X             calc-set-xor
;; v M-[             calc-vector-brackets
;; v M-]             calc-matrix-brackets
;; v M-^             calc-set-intersect
;; v M-a             calc-arrange-vector
;; v M-b             calc-build-vector
;; v M-c             calc-mcol
;; v M-d             calc-diag
;; v M-e             calc-expand-vector
;; v M-f             calc-vector-find
;; v M-h             calc-head
;; v M-i             calc-ident
;; v M-k             calc-cons
;; v M-l             calc-vlength
;; v M-m             calc-mask-vector
;; v M-n             calc-rnorm
;; v M-p             calc-pack
;; v M-r             calc-mrow
;; v M-s             calc-subvector
;; v M-t             calc-transpose
;; v M-u             calc-unpack
;; v M-v             calc-reverse-vector
;; v M-x             calc-index
;; v M-{             calc-vector-braces
;; v M-}             calc-matrix-brackets
;; v M-~             calc-set-complement
;; v N               calc-cnorm
;; v O               calc-outer-product
;; v R               calc-reduce
;; v S               calc-sort
;; v T               calc-mtrace
;; v U               calc-accumulate
;; v V               calc-set-union
;; v X               calc-set-xor
;; v [               calc-vector-brackets
;; v ]               calc-matrix-brackets
;; v ^               calc-set-intersect
;; v a               calc-arrange-vector
;; v b               calc-build-vector
;; v c               calc-mcol
;; v d               calc-diag
;; v e               calc-expand-vector
;; v f               calc-vector-find
;; v h               calc-head
;; v i               calc-ident
;; v k               calc-cons
;; v l               calc-vlength
;; v m               calc-mask-vector
;; v n               calc-rnorm
;; v p               calc-pack
;; v r               calc-mrow
;; v s               calc-subvector
;; v t               calc-transpose
;; v u               calc-unpack
;; v v               calc-reverse-vector
;; v x               calc-index
;; v {               calc-vector-braces
;; v }               calc-matrix-brackets
;; v ~               calc-set-complement

;;-- end vector

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    February 01, 2024
;; Modified:   February 01, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; _default_bindings.el ends here
