;;; editor/window-control/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Transient Toggle"              "T"    #'jg-toggle-main
      (:prefix "w"
       :desc "Toggle Layout"               "|"     #'+jg-ui-window-layout-toggle
       :desc "Rotate Windows"              "\\"    #'+jg-ui-window-rotate-forward
       )
      (:prefix "b"
       :desc "Toggle narrowing"            "-"   #'+jg-ui-toggle-narrow-buffer
       )
      )

(map! :map jg-binding-vision-map
      :desc "Narrow"        "RET" #'+jg-ui-narrow-around-point
      )

;;-- highlight
(map! :map jg-binding-vision-map
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
(map! :map jg-binding-backward-general-motion-map
      :desc "Narrow"       "RET"  #'+jg-ui-narrowing-move-focus-backward
      :desc "Todo"          "t"   #'hl-todo-previous
      )

(map! :map jg-binding-forward-general-motion-map
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

;;-- treemacs
treemacs-project-map
treemacs-toggle-map
treemacs--fringe-indicator-bitmap
treemacs-workspace-map
treemacs-copy-map
treemacs-node-visit-map
evil-treemacs-state-local-map
treemacs-mode-map
evil-treemacs-state-map

;;-- end treemacs
