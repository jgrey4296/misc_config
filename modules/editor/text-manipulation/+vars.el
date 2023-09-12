;;; util/text/+vars.el -*- lexical-binding: t; -*-


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

;;-- spelling
(setq ispell-program-name (or (executable-find "aspell")
                              (executable-find "hunspell")
                              (executable-find "enchant-2")
                              (executable-find "ispell")
                              )

      ispell-extra-args (pcase (f-filename ispell-program-name)
                          ("aspell"   '("--sug-mode=ultra" "--run-together"))
                          ("hunspell" '())
                          ("enchant-2"  '())
                          ("ispell"   '())
                          )

      ispell-personal-dictionary (expand-file-name "templates/tools/ispell_english" doom-user-dir)
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
(spec-handling-add! auto-modesj
                    '(text-manip
                      ("LICENSE" . license-mode)
                      )
                   )

(spec-handling-add! flyspell-predicate
                    `(markdown-mode ,#'+markdown-flyspell-word-p)
                    `(gfm-mode ,#'+markdown-flyspell-word-p)
                    )

;;-- end specs
