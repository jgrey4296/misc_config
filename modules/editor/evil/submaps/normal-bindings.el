;;; +evil-normal-bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-normal-state-map ;; State Changes
      :desc "Emacs State"         "C-z"      #'evil-emacs-state
      :desc "Run Macro"           "@"        #'evil-execute-macro
      :desc "Force Normal State"  "<escape>" #'evil-force-normal-state
      :desc "Eval expression"     "\""       #'pp-eval-expression
      )
(map! :map jg-binding-normal-state-map ;; Insert+
      :desc "Insert Below"  "o"   #'evil-open-below
      :desc "Insert"        "i"   #'evil-insert
      :prefix ("I" . "Insert+")
      ;; SPC reserved for jg-insert-state
      :desc "From evil register"       "0"          #'counsel-evil-registers

       :desc "Insert Resume"            "!"          #'evil-insert-resume

       :desc "Insert"                   "i"          #'evil-insert
       :desc "Insert after"             "l"          #'evil-append
       :desc "Replace-State"            "r"          #'evil-replace-state

       :desc "Append Line"              "a"          #'evil-append-line
       :desc "Prepend Line"             "p"          #'evil-insert-line
       :desc "Sub Line"                 "s"          #'evil-change-whole-line

       :desc "Open Below"               "j"          #'evil-open-below
       :desc "Open Above"               "k"          #'evil-open-above

       :desc "Current file name"        "f"          #'+default/insert-file-path
       :desc "Current file path"        "F"   (cmd!! #'+default/insert-file-path t)
       :desc "Snippet"                  "S"          #'yas-insert-snippet
       :desc "From Minibuffer history"  "m"          #'counsel-minibuffer-history
       :desc "Unicode"                  "u"          #'insert-char
       :desc "From Kill Ring"           "y"          #'+default/yank-pop

       :desc "General Insert"           "|"          #'general-insert-call
       )
(map! :map jg-binding-normal-state-map ;; Visual
      :desc "Visual"             "V"        #'evil-visual-line
      :prefix ("v" . "Visual+")
      :desc "buffer"             "RET" (cmd! (evil-visual-state) (mark-whole-buffer))
      :desc "line"               "j"   #'evil-visual-line
      :desc "Block"              "k"   #'evil-visual-block
      :desc "char"               "l"   #'evil-visual-char
      :desc "Restore selection"  "h"   #'evil-visual-restore

      :desc "Inner Select"       "i" (cmd! (evil-visual-char) (set-transient-map jg-binding-inner-text-objects-map))
      :desc "Outer Select"       "o" (cmd! (evil-visual-char) (set-transient-map jg-binding-outer-text-objects-map))
      )
(map! :map jg-binding-normal-state-map ;; paste
      :desc "Paste After"        "p"   #'evil-paste-after
      :prefix ("P" . "Paste")
      :desc "Paste After"        "l"   #'evil-paste-after
      :desc "Paste Before"       "h"   #'evil-paste-before
      :desc "Reselect"           "v"   #'+evil/reselect-paste
      :desc "From Register"      "r"   #'evil-paste-from-register
      )
(map! :map jg-binding-normal-state-map ;; Commands
      :desc "Use Register"       "'"   #'evil-use-register
      :desc "Join"               "J"   #'evil-join
      ;; K
      :desc "Indent"             "TAB" #'indent-for-tab-command
      :desc "Evil-Ex"            ":"   #'evil-ex
      :desc "Invert Char"        "~"   #'evil-invert-char
      :desc "Delete-op"          "d"   #'evil-delete
      :desc "Delete-char"        "x"   #'evil-delete-char
      :desc "B-delete"           "X"   #'evil-delete-backward-char
      :desc "Redisplay"          "§"   (cmd! (redisplay t))
      :desc "Set Marker"         "m"   #'evil-set-marker
      :desc "Replace"            "r"   #'evil-replace

      :desc "Undo"               "u"   #'evil-undo
      :desc "Yank"               "y"   #'evil-yank
      :desc "Yank-line"          "Y"   #'evil-yank-line
      )
(map! :map jg-binding-normal-state-map ;; chords
      :desc "Repeat Pop"      "C-."          #'evil-repeat-pop
      :desc "Paste Pop Next"  "C-n"          #'evil-paste-pop-next
      :desc "Paste Pop"       "C-p"          #'evil-paste-pop
      :desc "Redo"            "C-r"          #'evil-redo
      :desc "Pop Tag Mark"    "C-t"          #'pop-tag-mark
      :desc "Repeat Pop Next" "M-."          #'evil-repeat-pop-next
      :desc "Paste Pop"       "M-y"          #'evil-paste-pop
      :desc "Delete"          "<deletechar>" #'evil-delete-char
      :desc "Back Char"       "DEL"          #'evil-backward-char

      "C-f" #'evil-scroll-page-down
      "C-b" #'evil-scroll-page-up
      )
