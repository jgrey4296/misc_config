;;; +bind-motion.el -*- lexical-binding: t; no-byte-compile: t; -*-

(map! :map jgb-jump--text-map
      :desc "Force Goto Column" "\\" #'+jg-text-force-column-motion
      :desc "Random Line "      "?" #'+jg-text-goto-random-line-op
      )

(map! :map jge-txtobj--inner-map
      :desc "Empty lines"  "l" #'+jg-text-blank-block
      )

(map! :map jge-txtobj--outer-map
      :desc "Spaces"       "l" #'+jg-evil-whitespace
      )

(map! :map jgb-motion--text-f-map
      :desc "Next Section"            "]" #'evil-forward-section-begin
      :desc "To Section End"          "[" #'+evil/next-end-of-method
      :desc "Arg"                     "a" #'evil-forward-arg
      :desc "Next File in Dir, alpha" "f" #'+evil/next-file
      :desc "Heading"                 "h" #'outline-next-visible-heading
      :desc "Begin Method"            "m" #'+evil/next-beginning-of-method
      :desc "End Method"              "M" #'+evil/next-end-of-method
      :desc "Todo"                    "t" #'hl-todo-next
      :desc "Buffer"                  "b" #'next-buffer
      :desc "Comment"                 "c" #'+evil/next-comment
      :desc "Error"                   "e" #'next-error
      :desc "Paragraph"               "p" #'forward-paragraph
      )

(map! :map jgb-motion--text-b-map
      :desc "Section"                     "[" #'evil-backward-section-begin
      :desc "Section End"                 "]" #'evil-backward-section-end
      :desc "Buffer"                      "b" #'previous-buffer
      :desc "Comment"                     "c" #'+evil/previous-comment
      :desc "Previous File in Dir, alpha" "f" #'+evil/previous-file
      :desc "Error"                       "e" #'previous-error
      :desc "Heading"                     "h" #'outline-previous-visible-heading
      :desc "Begin Method"                "m" #'+evil/previous-beginning-of-method
      :desc "End Method"                  "M" #'+evil/previous-end-of-method
      :desc "Todo"                        "t" #'hl-todo-previous
      :desc "Paragraph"                   "p" #'backward-paragraph
      )

(map! :map jge-motion--op-bw-map
      :desc "Close Paren"  "]"   #'+jg-text-prev-close-paren-motion
      :desc "Empty Line"   "l"   #'+jg-text-prev-empty-line-motion
      )

(map! :map jge-motion--op-fw-map
      :desc "Open Section" "["   #'+jg-text-next-open-paren-motion
      :desc "Empty Line"   "l"   #'+jg-text-next-empty-line-motion
      )

;;; +bind-motion.el ends here
