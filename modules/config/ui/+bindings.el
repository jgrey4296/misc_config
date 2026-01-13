;;; editor/window-control/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Transient Toggle"              "T"    #'jg-toggle-main
      :desc "Project Find File"            "p RET" #'+jg-ui-tree/find-this-file
      (:prefix "h u"
       :desc "Choose modeline"              "0" #'+jg-ui-modeline-choose
       )
      )

(map! :leader :prefix ("w" . "Windows")
      ;; RET - workspace counsel
      :desc "Cleanup Frames"               "c"     #'+jg-ui-cleanup-frames
      :desc "Neotree Sidebar"              "s"     #'+jg-ui-tree/open
      :desc "Balance"                      "b"     #'balance-windows
      :desc "Delete Window"                "d"     #'delete-window
      :desc "Maximize"                     "m"     #'doom/window-maximize-buffer
      :desc "Undo window config"           "u"     #'winner-undo
      :desc "Redo window config"           "U"     #'winner-redo
      :desc "Window left"                  "h"     #'evil-window-left
      :desc "Window right"                 "j"     #'evil-window-down
      :desc "Window right"                 "l"     #'evil-window-right
      :desc "Window up"                    "k"     #'evil-window-up

      :desc "Split Below"                  "-"     #'split-window-below
      :desc "Split To Right"               "/"     #'split-window-right
      :desc "Shrink Horizontal"            "{"     #'shrink-window-horizontally
      :desc "Shrink Vertical"              "}"     #'shrink-window
       :desc "Toggle Layout"               "|"     #'+jg-ui-window-layout-toggle
       :desc "Rotate Windows"              "\\"    #'+jg-ui-window-rotate-forward
      )

(map! :map jgb-vision-text-map
      :desc "Toggle narrowing"            "="   #'+jg-ui-toggle-narrow-buffer
      :desc "Indirect Narrow"             "RET" #'+jg-ui-indirect-narrow-around-point
      :desc "Refresh Highlighting"        "u"   #'+jg-ui-refresh-highlighting
      :desc "Narrow"                      "n"   #'narrow-to-region
      )

;;-- highlight
(map! :map jgb-vision-text-map
      :desc "Delete Change Highlight"      "c" #'highlight-changes-remove-highlight
      :prefix ("'" . "Highlight")
       :desc  "symbol-at-point"            "." #'hi-lock-face-symbol-at-point
       :desc  "find-patterns"              "f" #'hi-lock-find-patterns
       :desc  "write-interactive-patterns" "i" #'hi-lock-write-interactive-patterns
       :desc  "lines-matching-regexp"      "l" #'hi-lock-line-face-buffer
       :desc  "phrase"                     "p" #'hi-lock-face-phrase-buffer
       :desc  "regexp"                     "r" #'hi-lock-face-buffer
       :desc  "unhighlight-regexp"         "u" #'hi-lock-unface-buffer
       :desc "changes"                     "c" #'highlight-changes-visible-mode
      )

;;-- end highlight

;;-- motion
(map! :map jge-b-motion-map
      :desc "Narrow"       "RET"  #'+jg-ui-narrowing-move-focus-backward
      :desc "Todo"          "t"   #'hl-todo-previous
      )

(map! :map jgb-f-motion-map
      :desc "Narrow"       "RET"  #'+jg-ui-narrowing-move-focus-forward
      :desc "Todo"         "t"   #'hl-todo-next
)
;;-- end motion

;;-- misc
(map! :map messages-buffer-mode-map
      :after message
      :desc  "backward-word-begin"   "b"              #'evil-backward-word-begin
      :desc  "forward-word-end"      "e"              #'evil-forward-word-end
      :desc  "find-char"             "f"              #'evil-find-char
      :desc  "backward-char"         "h"              #'evil-backward-char
      :desc  "next-line"             "j"              #'evil-next-line
      :desc  "previous-line"         "k"              #'evil-previous-line
      :desc  "forward-char"          "l"              #'evil-forward-char
      :desc  "ex-search-next"        "n"              #'evil-ex-search-next
      :desc  "find-char-to"          "t"              #'evil-find-char-to
      :desc  "visual-char"           "v"              #'evil-visual-char
      :desc  "forward-word-begin"    "w"              #'evil-forward-word-begin
      )

;;-- end misc

(local-load! "+bind-neotree")
(local-load! "+bind-treemacs")
