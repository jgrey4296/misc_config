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

;;-- spelling
(setq ispell-program-name (executable-find "aspell")
      ispell-extra-args '("--sug-mode=ultra" "--run-together")
      ispell-personal-dictionary (expand-file-name "spelling/aspell.en.pws" templates-loc)
      ispell-current-personal-dictionary ispell-personal-dictionary
      spell-fu-directory (concat doom-data-dir "spell-fu")
      flyspell-popup-correct-delay 0.8

      flyspell-lazy-idle-seconds 1
      flyspell-lazy-window-idle-seconds 3

      flyspell-issue-welcome-flag nil
      ;; Significantly speeds up flyspell, which would otherwise print
      ;; messages for every word when checking the entire buffer
      flyspell-issue-message-flag nil
      )

;;-- end spelling

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
(speckler-add! flyspell-predicate ()
  `(markdown-mode ,#'+markdown-flyspell-word-p)
  `(gfm-mode ,#'+markdown-flyspell-word-p)
  )
;;-- end specs
