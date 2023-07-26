;;; editor/window-control/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Transient Toggle"              "T"    #'jg-toggle-main
      (:prefix "w"
       :desc "Toggle Layout"               "|"     #'+jg-ui-window-layout-toggle
       :desc "Rotate Windows"              "\\"    #'+jg-ui-window-rotate-forward
       :desc "Dedicate"                    "."     #'+jg-ui-toggle-window-dedication
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

(map! :map emacs-lisp-mode-map
      :localleader
      :prefix ("i" . "Insert")
      :desc "Insert Palette Faces" "c" #'+jg-ui-insert-faces
      )
;;-- end misc

;;-- transient
;; transient-map transient-base-map transient-edit-map
(map! :map transient-toggles-minor-mode-map
      :n "T" #'jg-toggle-main
      )

(evil-make-overriding-map transient-toggles-minor-mode-map)
;;-- end transient

;;-- tree-sitter

(defvar tree-sitter-mode-map (make-sparse-keymap))

(defvar +tree-sitter-inner-text-objects-map (make-sparse-keymap))

(defvar +tree-sitter-outer-text-objects-map (make-sparse-keymap))

(defvar +tree-sitter-goto-previous-map (make-sparse-keymap))

(defvar +tree-sitter-goto-next-map (make-sparse-keymap))

(evil-define-key 'normal 'tree-sitter-mode
  "[g" +tree-sitter-goto-previous-map
  "]g" +tree-sitter-goto-next-map
  )
(map! :map tree-sitter-mode-map
      (:prefix "i"
               :vo "A" (+tree-sitter-get-textobj '("parameter.inner" "call.inner"))
               :vo "f" (+tree-sitter-get-textobj "function.inner")
               :vo "F" (+tree-sitter-get-textobj "call.inner")
               :vo "C" (+tree-sitter-get-textobj "class.inner")
               :vo "v" (+tree-sitter-get-textobj "conditional.inner")
               :vo "l" (+tree-sitter-get-textobj "loop.inner")
        )
      (:prefix "o"
               :vo "A" (+tree-sitter-get-textobj '("parameter.outer" "call.outer"))
               :vo "f" (+tree-sitter-get-textobj "function.outer")
               :vo "F" (+tree-sitter-get-textobj "call.outer")
               :vo "C" (+tree-sitter-get-textobj "class.outer")
               :vo "c" (+tree-sitter-get-textobj "comment.outer")
               :vo "v" (+tree-sitter-get-textobj "conditional.outer")
               :vo "l" (+tree-sitter-get-textobj "loop.outer")
               )
      (:prefix "[g"
            :n "a" (+tree-sitter-goto-textobj "parameter.outer" t)
            :n "f" (+tree-sitter-goto-textobj "function.outer" t)
            :n "F" (+tree-sitter-goto-textobj "call.outer" t)
            :n "C" (+tree-sitter-goto-textobj "class.outer" t)
            :n "c" (+tree-sitter-goto-textobj "comment.outer" t)
            :n "v" (+tree-sitter-goto-textobj "conditional.outer" t)
            :n "l" (+tree-sitter-goto-textobj "loop.outer" t)
            )
      (:prefix "]g"
            :n "a" (+tree-sitter-goto-textobj "parameter.outer")
            :n "f" (+tree-sitter-goto-textobj "function.outer")
            :n "F" (+tree-sitter-goto-textobj "call.outer")
            :n "C" (+tree-sitter-goto-textobj "class.outer")
            :n "c" (+tree-sitter-goto-textobj "comment.outer")
            :n "v" (+tree-sitter-goto-textobj "conditional.outer")
            :n "l" (+tree-sitter-goto-textobj "loop.outer")
      )
)
;;-- end tree-sitter
