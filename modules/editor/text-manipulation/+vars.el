;;; util/text/+vars.el -*- lexical-binding: t; -*-

(defvar-local jg-text-whitespace-clean-hook '(#'delete-trailing-whitespace
                                              #'+jg-text-cleanup-whitespace)
  )

(defvar jg-text-last-similarity-arg 1)

(defvar jg-text-debug-snippet-name "util.debug")

;;-- smartparens
(setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-pair-overlay-keymap (make-sparse-keymap)
        sp-max-prefix-length 25
        sp-max-pair-length 4
        )
;;-- end smartparens

;;-- invisibility
(setq-default buffer-invisibility-spec
              '((jg-text-invis . t)
                t
                )
              )

;;-- end invisibility

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

;;-- hl todo
(setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))
        )
;;-- end hl todo

;;-- undo
(setq-default
  ;; Increase undo history limits to reduce likelihood of data loss
 ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
 ;; truncating the undo history and corrupting the tree. See
 ;; https://github.com/syl20bnr/spacemacs/issues/12110
 undo-limit 800000
 undo-strong-limit 12000000
 undo-outer-limit 120000000
 )
;;-- end undo

;;-- highlight indent guides
(setq highlight-indent-guides-method 'character)
;;-- end highlight indent guides

;;-- ws butler
;; ws-butler normally preserves whitespace in the buffer (but strips it from
;; the written file). While sometimes convenient, this behavior is not
;; intuitive. To the average user it looks like whitespace cleanup is failing,
;; which causes folks to redundantly install their own.
(setq ws-butler-keep-whitespace-before-point nil)
;;-- end ws butler
