;;; editor/window-control/+bindings.el -*- lexical-binding: t; -*-

(map! :after jg-leader-bindings-loaded
      :leader
      :desc "Open project scratch buffer"  "p x"   #'+jg-ui-open-scratch-buffer
      (:prefix "b"
       :desc "Undo-Tree"                   "u"     #'+jg-ui-undo-tree
       :desc "Clear Popup Rules"           "P"     #'+jg-ui-ivy-reset-popup-rules
       )
      (:prefix "w"
       :desc "Toggle Layout"               "|"     #'+jg-ui-window-layout-toggle
       :desc "Rotate Windows"              "\\"    #'+jg-ui-window-rotate-forward
       )
      )

(map! :map jg-binding-helm-map
      :after jg-evil-bindings
      :desc "Insert Color"                 "c"     #'helm-colors
      )

(map! :map jg-binding-vision-map
      :after jg-evil-bindings
      :desc "Narrow"        "RET" #'+jg-ui-narrow-around-point
      )

(map! :map window-ring-edit-map
      "C-c C-c" #'window-ring-edit-commit)


;;-- window ring
(map! :after jg-leader-bindings-loaded
      :leader
      :prefix ("w r" . "Ring")
      :desc "Pop Buffer (Alt: Pop to here)"  "c" #'window-ring-pop-buffer
      :desc "Clear Ring"                     "C" #'window-ring-clear-ring
      :desc "Edit Ring"                      "e" #'window-ring-edit-order
      :desc "Ring Right"                     "l" #'window-ring-move-perspective
      :desc "Ring Left"                      "h" #'window-ring-move-perspective-2
      :desc "Most Recent"                    "L" #'window-ring-goto-most-recent
      :desc "Oldest"                         "H" #'window-ring-goto-oldest

      :desc "Add Current Buffer"             "b" #'window-ring-add-current-buffer
      :desc "Find File -> Head"              "f" #'window-ring-add-to-head
      :desc "Find File -> Tail"              "F" #'window-ring-add-to-tail

      :desc "Print Sequence"                 "p" #'window-ring-print-order

      :desc "Window Ring soft Reset"         "s" #'+jg-ui-window-ring-block-reset
      :desc "Window Ring Hard Reset"         "S" #'window-ring-setup-columns-command
      :desc "Remove Current Buffer"          "R" #'window-ring-remove-buffer
      :desc "Replace with Buffer"            "r" #'window-ring-replace-buffer

      :desc "Toggle Ring Loop"               "q" #'(lambda () (interactive) (setq window-ring-can-loop (not window-ring-can-loop)))

      :desc "Shrink Side Wndows"             "{" #'window-ring-shrink-sides
      )
;;-- end window ring

;;-- highlight
(map! :map jg-binding-vision-map
      :after jg-evil-bindings
      :prefix ("'" . "Highlight")
       :desc  "symbol-at-point"            "." #'hi-lock-face-symbol-at-point
       :desc  "find-patterns"              "f" #'hi-lock-find-patterns
       :desc  "write-interactive-patterns" "i" #'hi-lock-write-interactive-patterns
       :desc  "lines-matching-regexp"      "l" #'hi-lock-line-face-buffer
       :desc  "phrase"                     "p" #'hi-lock-face-phrase-buffer
       :desc  "regexp"                     "r" #'hi-lock-face-buffer
       :desc  "unhighlight-regexp"         "u" #'hi-lock-unface-buffer
      )

;;-- end highlight

;;-- motion
(map! :map jg-binding-backward-general-motion-map
      :after jg-evil-bindings
      :desc "Ring Window"  "r"    #'window-ring-move-perspective-2
      :desc "Narrow"       "RET"  #'+jg-ui-narrowing-move-focus-backward
      )

(map! :map jg-binding-forward-general-motion-map
      :after jg-evil-bindings
      :desc "Narrow"       "RET"  #'+jg-ui-narrowing-move-focus-forward
      :desc "Ring Window"  "r"    #'window-ring-move-perspective
)
;;-- end motion

;;-- misc
(map! :map messages-buffer-mode-map
      :after message
      :n "q" #'+popup/close

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

(map! :map emacs-lisp-mode-map
      :localleader
      :prefix ("i" . "Insert")
      :desc "Insert Palette Faces" "c" #'+jg-ui-insert-faces
      )
;;-- end misc
