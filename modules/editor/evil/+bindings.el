;;; editor/evil/+bindings.el -*- lexical-binding: t; -*-

(map! :v  "@"     #'+evil:apply-macro
      :m  [C-i]   #'evil-jump-forward
      ;; ported from vim-unimpaired
      :n  "] SPC" #'+evil/insert-newline-below
      :n  "[ SPC" #'+evil/insert-newline-above
      :n  "] b"   #'next-buffer
      :n  "[ b"   #'previous-buffer
      :n  "] f"   #'+evil/next-file
      :n  "[ f"   #'+evil/previous-file
      :m  "] u"   #'+evil:url-encode
      :m  "[ u"   #'+evil:url-decode
      :m  "] y"   #'+evil:c-string-encode
      :m  "[ y"   #'+evil:c-string-decode
      ;; custom vim-unmpaired-esque keys
      :m  "] #"   #'+evil/next-preproc-directive
      :m  "[ #"   #'+evil/previous-preproc-directive

      :m  "] a"   #'evil-forward-arg
      :m  "[ a"   #'evil-backward-arg

      :m  "] c"   #'+evil/next-comment
      :m  "[ c"   #'+evil/previous-comment

      :m  "] e"   #'next-error
      :m  "[ e"   #'previous-error

      :n  "] F"   #'+evil/next-frame
      :n  "[ F"   #'+evil/previous-frame

      :m  "] h"   #'outline-next-visible-heading
      :m  "[ h"   #'outline-previous-visible-heading

      :m  "] m"   #'+evil/next-beginning-of-method
      :m  "[ m"   #'+evil/previous-beginning-of-method

      :m  "] M"   #'+evil/next-end-of-method
      :m  "[ M"   #'+evil/previous-end-of-method

      :n  "[ o"   #'+evil/insert-newline-above
      :n  "] o"   #'+evil/insert-newline-below

      :n  "g p"   #'+evil/reselect-paste
      :v  "g p"   #'+evil/alt-paste
      :nv "g@"    #'+evil:apply-macro
      :nv "g c"   #'evilnc-comment-operator
      :nv "g x"   #'evil-exchange
      :nv "g y"   #'+evil:yank-unindented
      :n  "g="    #'evil-numbers/inc-at-pt
      :n  "g-"    #'evil-numbers/dec-at-pt
      :v  "g="    #'evil-numbers/inc-at-pt-incremental
      :v  "g-"    #'evil-numbers/dec-at-pt-incremental
      :v  "g+"    #'evil-numbers/inc-at-pt
      ;; custom evil keybinds
      :nv "z n"   #'+evil:narrow-buffer
      :n  "z N"   #'doom/widen-indirectly-narrowed-buffer
      :n  "z x"   #'kill-current-buffer
      :n  "Z X"   #'doom/save-and-kill-buffer
      ;; don't leave visual mode after shifting
      :v  "<"     #'+evil/shift-left  ; vnoremap < <gv
      :v  ">"     #'+evil/shift-right  ; vnoremap > >gv

      ;; text objects
      :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
      :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
      :textobj "c" #'evilnc-inner-comment              #'evilnc-outer-commenter
      :textobj "f" #'+evil:defun-txtobj                #'+evil:defun-txtobj
      :textobj "g" #'+evil:whole-buffer-txtobj         #'+evil:whole-buffer-txtobj
      :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
      :textobj "j" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down
      :textobj "k" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
      :textobj "q" #'+evil:inner-any-quote             #'+evil:outer-any-quote
      :textobj "u" #'+evil:inner-url-txtobj            #'+evil:outer-url-txtobj
      :textobj "x" #'evil-inner-xml-attr               #'evil-outer-xml-attr

      ;; evil-surround
      :v "S" #'evil-surround-region
      :o "s" #'evil-surround-edit
      :o "S" #'evil-Surround-edit

      ;; evil-lion
      :n "g l" #'evil-lion-left
      :n "g L" #'evil-lion-right
      :v "g l" #'evil-lion-left
      :v "g L" #'evil-lion-right

      )
;; implement dictionary keybinds
;; evil already defines 'z=' to `ispell-word' = correct word at point
(map! :when (featurep! :checkers spell)
      :n  "z g"   #'+spell/add-word
      :n  "z w"   #'+spell/remove-word
      :m  "[ s"   #'+spell/previous-error
      :m  "] s"   #'+spell/next-error)
(map! :when (featurep! :lang web)
      :m "] x"   #'+web:encode-html-entities
      :m "[ x"   #'+web:decode-html-entities)
(map! :when (featurep! :ui vc-gutter)
      :m "] d"   #'git-gutter:next-hunk
      :m "[ d"   #'git-gutter:previous-hunk)
(map! :when (featurep! :ui hl-todo)
      :m "] t"   #'hl-todo-next
      :m "[ t"   #'hl-todo-previous)
(map! :when (featurep! :ui workspaces)
      :n "g t"   #'+workspace:switch-next
      :n "g T"   #'+workspace:switch-previous
      :n "] w"   #'+workspace/switch-right
      :n "[ w"   #'+workspace/switch-left)
(map! :when (featurep! :ui tabs)
      :n "g t"   #'centaur-tabs-forward
      :n "g T"   #'centaur-tabs-backward)
(map! :when (featurep! :tools lookup)
      :nv " K"   #'+lookup/documentation
      :nv "g d"  #'+lookup/definition
      :nv "g D"  #'+lookup/references
      :nv "g f"  #'+lookup/file
      :nv "g I"  #'+lookup/implementations
      :nv "g A"  #'+lookup/assignments)
(map! :when (featurep! :tools eval)
      :nv "g r"  #'+eval:region
      :n  "g R"  #'+eval/buffer
      :v  "g R"  #'+eval:replace-region
      ;; Restore these keybinds, since the blacklisted/overwritten gr/gR will
      ;; undo them:
      (:after dired
       :map dired-mode-map
       :n "g r" #'revert-buffer)
      (:after notmuch
       :map notmuch-common-keymap
       :n "g r" #'notmuch-refresh-this-buffer
       :n "g R" #'notmuch-poll-and-refresh-this-buffer)
      (:after elfeed
       :map elfeed-search-mode-map
       :n "g r" #'elfeed-search-update--force
       :n "g R" #'elfeed-search-fetch))
;; window management (prefix "C-w")
(map! :map evil-window-map
      ;; Navigation
      "C-h"     #'evil-window-left
      "C-j"     #'evil-window-down
      "C-k"     #'evil-window-up
      "C-l"     #'evil-window-right
      "C-w"     #'other-window
      ;; Extra split commands
      "S"       #'+evil/window-split-and-follow
      "V"       #'+evil/window-vsplit-and-follow
      ;; Swapping windows
      "H"       #'+evil/window-move-left
      "J"       #'+evil/window-move-down
      "K"       #'+evil/window-move-up
      "L"       #'+evil/window-move-right
      "C-S-w"   #'ace-swap-window
      ;; Window undo/redo
      (:prefix "m"
       "m"       #'doom/window-maximize-buffer
       "v"       #'doom/window-maximize-vertically
       "s"       #'doom/window-maximize-horizontally)
      "u"       #'winner-undo
      "C-u"     #'winner-undo
      "C-r"     #'winner-redo
      "o"       #'doom/window-enlargen
      ;; Delete window
      "d"       #'evil-window-delete
      "C-C"     #'ace-delete-window
      "T"       #'tear-off-window)
;; evil-easymotion (see `+evil/easymotion')
(map! :after evil-easymotion
  :m "gs" evilem-map
  (:map evilem-map
   "a" (evilem-create #'evil-forward-arg)
   "A" (evilem-create #'evil-backward-arg)
   "s" #'evil-avy-goto-char-2
   "SPC" (cmd! (let ((current-prefix-arg t)) (evil-avy-goto-char-timer)))
   "/" #'evil-avy-goto-char-timer))
 ;; evil-snipe
(map! :after evil-snipe
  :map evil-snipe-parent-transient-map
  "C-;" (cmd! (require 'evil-easymotion)
              (call-interactively
               (evilem-create #'evil-snipe-repeat
                              :bind ((evil-snipe-scope 'whole-buffer)
                                     (evil-snipe-enable-highlight)
                                     (evil-snipe-enable-incremental-highlight))))))
;; Omni-completion
(map! :when (featurep! :completion company)
      :prefix "C-x"
       :i "C-l"    #'+company/whole-lines
       :i "C-k"    #'+company/dict-or-keywords
       :i "C-f"    #'company-files
       :i "C-]"    #'company-etags
       :i "s"      #'company-ispell
       :i "C-s"    #'company-yasnippet
       :i "C-o"    #'company-capf
       :i "C-n"    #'+company/dabbrev
       :i "C-p"    #'+company/dabbrev-code-previous)
