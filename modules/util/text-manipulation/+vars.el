;;; util/text/+vars.el -*- lexical-binding: t; -*-
(setq-default jg-text-last-similarity-arg 1
              jg-text-debug-snippet-name "util.debug"
              )

;;-- evil-surround
(setq-default evil-surround-pairs-alist
  '((?\( . ("( " . " )"))
    (?\[ . ("[ " . " ]"))
    (?\{ . ("{ " . " }"))

    (?\) . ("(" . ")"))
    (?\] . ("[" . "]"))
    (?\} . ("{" . "}"))

    (?# . ("#{" . "}"))
    (?b . ("(" . ")"))
    (?p . ("(" . ")"))
    (?B . ("{" . "}"))
    (?> . ("<" . ">"))
    (?t . evil-surround-read-tag)
    (?< . evil-surround-read-tag)
    (?f . evil-surround-function))
)

;;-- end evil-surround

;;-- invisibility
(setq-default buffer-invisibility-spec
              '((jg-text-invis . t)
                t
                )
              )

;;-- end invisibility

;;-- visual line hook
(defun disable-vl-mode ()
  (interactive)
  (visual-line-mode -1)
  )

(add-hook! 'text-mode-hook :append #'disable-vl-mode)

;;-- end visual line hook

;;-- rotate-text
(setq-default rotate-text-words '(("enable" "disable")
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
                                 )

              )

;;-- end rotate-text
