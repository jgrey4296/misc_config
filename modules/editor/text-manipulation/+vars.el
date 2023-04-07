;;; util/text/+vars.el -*- lexical-binding: t; -*-

(defvar jg-text-whitespace-clean-hook nil)

(add-hook! 'jg-text-whitespace-clean-hook
           #'delete-trailing-whitespace
           #'+jg-text-cleanup-whitespace 50)

(setq-default jg-text-last-similarity-arg 1
              jg-text-debug-snippet-name "util.debug"
              )

(add-hook 'lint-result-mode-hook '+fold/close-all)

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
                                 ("published" "draft")
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
                                 )

              )

;;-- end rotate-text

(after! evil
  (setq-default evil-shift-width 4)
  (remove-hook! 'after-change-major-mode-hook #'doom--setq-evil-shift-width-for-after-change-major-mode-h)
  )
