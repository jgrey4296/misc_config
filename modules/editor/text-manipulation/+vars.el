;;; util/text/+vars.el -*- lexical-binding: t; -*-

(setq-default tab-always-indent t
              indent-tabs-mode nil
              )

;;-- smartparens
(speckler-setq! smartparens ()
  sp-highlight-pair-overlay nil
  sp-highlight-wrap-overlay t
  sp-highlight-wrap-tag-overlay t
  sp-show-pair-from-inside t
  sp-navigate-skip-match nil
  sp-navigate-consider-sgml-tags nil
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
(speckler-setq! rotate-text ()
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
                      ("off", "on")
                      )
  rotate-text-symbols '(("∃" "∀" "∄" "∈" )
                        ("∧" "∨")
                        ("⟘" "⟙")
                        ("□" "◇")
                        ("♭" "♯")
                        ("⊨" "⇒" "⊢")
                        ("↑" "↓" "↕")
                        ("==" "!=")
                        ("+" "-" "*" "/" "_")
                        ("⨁" "⨂" "⊸")
                        ( "₁" "₂" "₃" "₄" "₅" "₆" "₇" "₈" "₉" "₀" "ᵢ" "ⱼ")
                        ( "¹" "²" "³" "⁴" "⁵" "⁶" "⁷" "⁸" "⁹" "ⁱ")
                        ("default" "default_factory")
                        )
  )
(speckler-setq! evil-shift ()
  evil-shift-width 4
  )
(speckler-add! auto-modes ()
  '(text-manip
    ("LICENSE" . license-mode)
    )
  )
(speckler-add! evil-ex ()
  '(text-manip
    ("hl"            . #'+jg-text-manipulation-ex-match-highlight)
    ("hlc"           . #'evil-ex-match)

    ("rev[erse]"     . #'+evil:reverse-lines)
    ("l[ine]diff"    . #'evil-quick-diff)
    ("ld"            . #'evil-quick-diff)

    ("c[opy]"        . #'evil-copy)
    ("m[ove]"        . #'evil-move)
    ("d[elete]"      . #'evil-ex-delete)
    ("y[ank]"        . #'evil-ex-yank)

    ("j[oin]"        . #'evil-ex-join)

    ("a[lign]"       . #'+jg-text-manipulation-ex-align-highlight)
    ("A"             . #'+jg-text-manipulation-ex-expand-align-highlight)
    (":"             . #'+jg-text-manipulation-ex-expand-align-highlight)
    ("::"            . #'+jg-text-manipulation-ex-auto-align)
    ("la"            . #'evil-align-left)
    ("ra"            . #'evil-align-right)
    ("ce[nter]"      . #'evil-align-center)
    ("<"             . #'evil-shift-left)
    (">"             . #'evil-shift-right)

    ("s[ubstitute]"  . #'evil-ex-substitute)
    ("S[ubstitute]"  . #'evil-ex-substitute)
    ("sr"            . #'evil-ex-repeat-substitute)
    ("srf"           . #'evil-ex-repeat-substitute-with-flags)

    ("show-digraphs" . #'evil-ex-show-digraphs)
    ("sor[t]"        . #'evil-ex-sort)

    ("sel"           . #'+jg-text-manipulation-ex-expand-selection)
    )
  )
(speckler-setq! evil-ex-auto-align ()
  ;; Sequence of regexps to auto align by
  jg-text-manip-auto-align-groups '("/:/" "/=/" "/#")
  )

;;-- end specs
