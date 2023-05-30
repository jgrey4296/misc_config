;;; util/text/+vars.el -*- lexical-binding: t; -*-

(defvar-local jg-text-whitespace-clean-hook '(#'delete-trailing-whitespace
                                              #'+jg-text-cleanup-whitespace)
  )

(defvar jg-text-last-similarity-arg 1)

(defvar jg-text-debug-snippet-name "util.debug")

(setq-default tab-always-indent t
              indent-tabs-mode nil
              )

;;-- smartparens
(setq sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay t
      sp-highlight-wrap-tag-overlay t
      sp-show-pair-from-inside t
      sp-cancel-autoskip-on-backward-movement nil
      sp-pair-overlay-keymap (make-sparse-keymap)
      sp-max-prefix-length 25
      sp-max-pair-length 4
      )

(setq-default sp-autoinsert-pair t
              sp-autoinsert-quote-if-followed-by-closing-pair nil
              sp-escape-char ""
)
;;-- end smartparens

;;-- invisibility
(setq-default buffer-invisibility-spec
              '((jg-text-invis . t)
                t
                )
              )

;;-- end invisibility

;;-- ws butler
;; ws-butler normally preserves whitespace in the buffer (but strips it from
;; the written file). While sometimes convenient, this behavior is not
;; intuitive. To the average user it looks like whitespace cleanup is failing,
;; which causes folks to redundantly install their own.
(setq ws-butler-keep-whitespace-before-point nil)
;;-- end ws butler

;;-- specs
(spec-handling-setq! rotate-text
                     rotate-text-words '(("enable" "disable")
                                         ("true" "false")
                                         ("width" "height")
                                         ("left" "right" "top" "bottom")
                                         ("and" "or")
                                         ("open" "closed")
                                         ("why" "what" "who/whom" "when" "where" "how")
                                         ("position" "demarcation" "decision" "choice" "information" "payoff" "scope")
                                         ("retry" "break" "timeout" "isolate" "cache" "fallback")
                                         ("notset" "debug" "info" "warning" "error" "critical")
                                         ("millisecond" "second" "minute" "hour" "day" "week" "month" "year" "decade")
                                         ("i" "we")
                                         ("he" "she" "they" "one" "zie")
                                         ("him" "(obj)her" "them")
                                         ("his" "(pos)her" "their" "my" "your" "our")
                                         ("himself" "herself" "themself")
                                         ("assert" "direct" "commit" "express" "declare")
                                         ("major" "minor" "pentatonic" "diatonic" "chromatic")
                                         ("ionian" "dorian" "phrygian" "lydian" "mixolydian" "aeolian" "locrian")
                                         ("nominal" "ordinal" "interval" "ratio")
                                         ("exists" "all")
                                         ("dict" "set")
                                         ("journal" "booktitle")
                                         ("author" "editor")
                                         ("published" "draft")
                                         ("inclusive" "exclusive")
                                         ("prepend" "append")
                                         ("jg" "john grey")
                                         )
                     rotate-text-symbols '(("∃" "∀" "∄" "∈" )
                                           ("∧" "∨")
                                           ("⟘" "⟙")
                                           ("□" "◇")
                                           ("♭" "♯")
                                           ("⊨" "⇒" "⊢")
                                           ("↑" "↓" "↕")
                                           ("==" "!=")
                                           ("+" "-" "*" "/")
                                           ("⨁" "⨂" "⊸")
                                           ( "₁" "₂" "₃" "₄" "₅" "₆" "₇" "₈" "₉" "₀" "ᵢ" "ⱼ")
                                           ( "¹" "²" "³" "⁴" "⁵" "⁶" "⁷" "⁸" "⁹" "ⁱ")
                                           ("default" "default_factory")
                                           ("_" "-")
                                 )

              )
(spec-handling-setq! evil-shift
                     evil-shift-width 4
                     )

;;-- end specs
