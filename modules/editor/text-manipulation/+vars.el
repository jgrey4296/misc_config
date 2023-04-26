;;; util/text/+vars.el -*- lexical-binding: t; -*-

;;-- defs
(defvar +word-wrap-extra-indent 'double
  "The amount of extra indentation for wrapped code lines.

When 'double, indent by twice the major-mode indentation.
When 'single, indent by the major-mode indentation.
When a positive integer, indent by this fixed amount.
When a negative integer, dedent by this fixed amount.

Otherwise no extra indentation will be used.")

(defvar +word-wrap-fill-style nil
  "How to handle `fill-column' in `+word-wrap-mode'.

When 'auto, long lines will soft-wrap at `fill-column'. If `auto-fill-mode' is
enabled, its behaviour will not be affected.

When 'soft, long lines will soft-wrap at `fill-column' and `auto-fill-mode' will
be forcibly disabled.

Otherwise long lines will soft-wrap at the window margin and `auto-fill-mode'
will not be affected.")

(defvar +word-wrap-disabled-modes
  '(fundamental-mode so-long-mode)
  "Major-modes where `+global-word-wrap-mode' should not enable
`+word-wrap-mode'.")

(defvar +word-wrap-visual-modes
  '(org-mode)
  "Major-modes where `+word-wrap-mode' should not use
`adaptive-wrap-prefix-mode'.")

(defvar +word-wrap-text-modes
  '(text-mode markdown-mode markdown-view-mode gfm-mode gfm-view-mode rst-mode
    latex-mode LaTeX-mode)
  "Major-modes where `+word-wrap-mode' should not provide extra indentation.")

(when (memq 'visual-line-mode text-mode-hook)
  (remove-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'+word-wrap-mode))



;;-- end defs

(defvar-local jg-text-whitespace-clean-hook '(#'delete-trailing-whitespace
                                              #'+jg-text-cleanup-whitespace)
  )

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

(after! evil-quickscope
  (global-evil-quickscope-always-mode 1)
  )

(after! evil
  (remove-hook! 'after-change-major-mode-hook #'doom--setq-evil-shift-width-for-after-change-major-mode-h)
  )

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
(spec-handling-add! lookup-regular nil
                    '(dired-mode
                     ("Jq Manual" . "https://stedolan.github.io/jq/manual/")
                     )
                    )
