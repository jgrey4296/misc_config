;;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-

(global-set-key (kbd "C-c [") #'+jg-personal-insert-lparen)
(global-set-key (kbd "C-c ]") #'+jg-personal-insert-rparen)
(global-set-key (kbd "C-c u") #'universal-argument)

;; For minibuffer use:
(map! :map ctl-x-map
      "[" "("
      "]" ")")

(map! :g "C-x h" help-map)
(map! :map universal-argument-map
      :prefix doom-leader-key     "u"#'universal-argument-more
      :prefix doom-leader-alt-key "u"#'universal-argument-more)

;; Evil States
(map! :map evil-normal-state-map
      "Z" nil)
(map! :map evil-normal-state-map
      :prefix "z"
      "= "          #'ispell-word
      "A"          #'evil-open-fold-rec
      "C"          nil
      "D"          #'evil-close-folds
      "N"          nil ;; #'doom/widen-indirectly-narrowed-buffer
      "O"          nil
      "RET"        #'+jg-narrow-around-point
      "X"          nil ;; #'kill-current-buffer
      "a"          #'evil-toggle-fold
      "c"          nil
      "d"          #'evil-close-fold
      "m"          nil
      "n"          nil ;; #'+evil:narrow-buffer
      "o"          #'evil-open-fold
      "r"          #'evil-open-folds
      "s"          nil ;;#'evil-open-folds
      (:prefix ("'" . "Highlight")
      "' ."        #'highlight-symbol-at-point
      "' f"        #'hi-lock-find-patterns
      "' i"        #'hi-lock-write-interactive-patterns
      "' l"        #'highlight-lines-matching-regexp
      "' p"        #'highlight-phrase
      "' r"        #'highlight-regexp
      "' u"        #'unhighlight-regexp)
      (:prefix ("v" . "Vimish Fold")
      "v A"        #'vimish-fold-toggle-all
      "v D"        #'vimish-fold-delete-all
      "v a"        #'vimish-fold-toggle
      "v d"        #'vimish-fold-delete
      "v f"        #'vimish-fold
      "v j"        #'vimish-fold-next-fold
      "v k"        #'vimish-fold-previous-fold
      "v m"        #'vimish-fold-refold-all
      "v r"        #'vimish-fold-unfold-all
      "x"          nil)
)
(map! :map evil-normal-state-map
      :prefix "g"
      "&"           #'evil-ex-repeat-global-substitute
      ","           #'goto-last-change-reverse
      "-"           #'evil-numbers/dec-at-pt
      "8"           nil ;; #'what-cursor-position
      ";"           #'goto-last-change
      "="           #'evil-numbers/inc-at-pt
      ">"           #'evil-goto-column
      "?"           #'evil-rot13
      "@"           #'+evil:apply-macro
      "A"           #'+lookup/assignments
      "D"           #'+lookup/references
      "F"           #'evil-find-file-at-point-with-line
      "I"           #'+lookup/implementations
      "J"           #'evil-join-whitespace
      "L"           #'evil-lion-right
      "R"           nil ;; #'+eval/buffer
      "T"           nil ;; #'+workspace:switch-previous
      "U"           #'evil-upcase
      "a"           #'what-cursor-position
      "b"           #'avy-pop-mark
      "c"           #'evilnc-comment-operator
      "d"           #'+lookup/definition
      "f"           nil ;; #'+lookup/file
      "i"           #'evil-insert-resume
      "l"           #'evil-lion-left
      "p"           #'+evil/reselect-paste
      "q"           #'evil-fill-and-move
      "r"           nil ;; #'+eval:region
      "t"           nil ;; #'+workspace:switch-next
      "u"           #'evil-downcase
      "w"           #'evil-fill
      "x"           #'evil-exchange
      "y"           #'+evil:yank-unindented
      "~"           #'evil-invert-case

)

(map! :map evil-motion-state-map
      "TAB" nil
      "TAB"     #'indent-for-tab-command
      "\\"  nil
      "] RET"   #'+jg-narrowing-move-focus-forward
      "[ RET"   #'+jg-narrowing-move-focus-backward
      "g b"     #'avy-pop-mark
      "g >"     #'evil-goto-column
      "<left>"  #'evil-scroll-left
      "<right>" #'evil-scroll-right
      "<up>"    #'evil-scroll-page-up
      "<down>"  #'evil-scroll-page-down
      ;; "ESC <down>" #'(lambda () (interactive) (message "Test"))
)
(map! :map evil-motion-state-map
      :prefix "g"
      ;; TODO: count-words evil-first-non-blank-of-visual-line evil-last-non-blank
      "#"             nil ;; evil-ex-search-unbounded-word-backward
      "$"             nil ;; evil-end-of-visual-line
      "*"             #'evil-ex-search-unbounded-word-forward
      "0"             nil ;; evil-beginning-of-visual-line
      "C-]"           nil ;; evil-jump-to-tag
      "C-g "          nil ;; count-words
      "E "            nil ;; evil-backward-WORD-end
      "N "            nil ;; evil-previous-match
      "^ "            nil ;; evil-first-non-blank-of-visual-line
      "_ "            nil ;; evil-last-non-blank
      "d "            #'evil-goto-definition
      "e "            nil ;; evil-backward-word-end
      "f "            #'evil-goto-first-line
      "g "            nil ;; #'evil-goto-first-line
      "j "            #'evil-next-visual-line
      "k "            #'evil-previous-visual-line
      "m "            #'evil-middle-of-visual-line
      "v "            #'evil-visual-restore
      "n "            nil ;; #'evil-next-match
      "s "            nil
      "s # "          nil ;; #'evilem-motion-search-word-backward
      "s ( "          nil ;; #'evilem-motion-backward-sentence-begin
      "s ) "          nil ;; #'evilem-motion-forward-sentence-begin
      "s * "          nil ;; #'evilem-motion-search-word-forward
      "s + "          nil ;; #'evilem-motion-next-line-first-non-blank
      "s - "          nil ;; #'evilem-motion-previous-line-first-non-blank
      "s / "          nil ;; #'evil-avy-goto-char-timer
      "s A "          nil ;; #'evilem--motion-function-evil-backward-arg
      "s B "          nil ;; #'evilem-motion-backward-WORD-begin
      "s E "          nil ;; #'evilem-motion-forward-WORD-end
      "s F "          nil ;; #'evilem-motion-find-char-backward
      "s N "          nil ;; #'evilem-motion-search-previous
      "s SPC "        nil
      "s T "          nil ;; #'evilem-motion-find-char-to-backward
      "s W "          nil ;; #'evilem-motion-forward-WORD-begin
      "s ["           nil
      "s [ [ "        nil ;; #'evilem-motion-backward-section-begin
      "s [ ] "        nil ;; #'evilem-motion-backward-section-end
      "s ] "          nil
      "s ] [ "        nil ;; #'evilem-motion-forward-section-end
      "s ] ] "        nil ;; #'evilem-motion-forward-section-begin
      "s a "          nil ;; #'evilem--motion-function-evil-forward-arg
      "s b "          nil ;; #'evilem-motion-backward-word-begin
      "s e "          nil ;; #'evilem-motion-forward-word-end
      "s f "          nil ;; #'evilem-motion-find-char
      "s g E "        nil ;; #'evilem-motion-backward-WORD-end
      "s g e "        nil ;; #'evilem-motion-backward-word-end
      "s g j "        nil ;; #'evilem-motion-next-visual-line
      "s g k "        nil ;; #'evilem-motion-previous-visual-line
      "s j "          nil ;; #'evilem-motion-next-line
      "s k "          nil ;; #'evilem-motion-previous-line
      "s n "          nil ;; #'evilem-motion-search-next
      "s s "          nil ;; #'evil-avy-goto-char-2
      "s t "          nil ;; #'evilem-motion-find-char-to
      "s w "          nil ;; #'evilem-motion-forward-word-begin
)
(map! :map evil-motion-state-map
      :prefix "z"
      "n" nil
      "+" nil
      "-" nil
      "." nil
      "^" nil
      "n" nil
      "N" nil
      "o" nil
      "s" nil
      "X" nil
      :desc "Narrow" "RET" #'+jg-narrow-around-point
      :desc "Widen"  "DEL" #'widen
      )
;;
;; Shell
(map! :map shell-mode-map
      :localleader
      "h" #'counsel-shell-history)
;; Ibuffer
(map! :after ibuffer
      :mode ibuffer-mode
      "\\" ibuffer--filter-map
      )
;; Flycheck
(map! :after flycheck
      :map flycheck-error-list-mode-map
      :n "," nil
      :n "," #'tabulated-list-sort
      :n "{" #'tabulated-list-narrow-current-column
      :n "}" #'tabulated-list-widen-current-column
      )
;; Git Timemachine
(map! :map git-timemachine-mode-map
      :n "[ g" #'git-timemachine-show-previous-revision
      :n "] g" #'git-timemachine-show-next-revision
      )

;; Mouse Deactivation
(define-key evil-motion-state-map [down-mouse-1] #'ignore)
(define-key evil-motion-state-map [mouse-1] #'ignore)
(define-key evil-motion-state-map [drag-mouse-1] #'ignore)

(define-key evil-motion-state-map [down-mouse-2] #'ignore)
(define-key evil-motion-state-map [mouse-2] #'ignore)
(define-key evil-motion-state-map [drag-mouse-2] #'ignore)

(define-key evil-motion-state-map [down-mouse-3] #'ignore)
(define-key evil-motion-state-map [mouse-3] #'ignore)
(define-key evil-motion-state-map [drag-mouse-3] #'ignore)

(define-key evil-motion-state-map [mouse-4] #'ignore)
(define-key evil-motion-state-map [mouse-5] #'ignore)
