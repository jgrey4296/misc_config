;;; change-map.el -*- lexical-binding: t; no-byte-compile: t; -*-

(map! :map jgb-change--text-map
      (:prefix ("w" . "Words"))
      (:prefix ("e" . "Encoding"))
      (:prefix ("i" . "Lines"))
      (:prefix ("o" . "Text"))
      (:prefix ("S" . "Surround"))
      )

(map! :map jgb-change--text-map ;; General
      :desc "Invis"                       "z"          #'+jg-text-toggle-invisible
      :desc "split line"                  "RET"        #'electric-newline-and-maybe-indent
      :desc "Substitute Ex Memory"        ";"          #'+jg-text-manipulation-sub-memory
      :desc "set buffer coding"           "0"          #'set-buffer-file-coding-system
      :desc "indent"                      "TAB"        #'indent-region

      :desc "Ensure commas"               "u"          #'+jg-surround-ensure-commas

      :desc "Align"                       "a"          #'align-regexp
      :desc "Comment"                     "c"          #'evilnc-comment-operator
      :desc "Surround"                    "s"          #'evil-surround-region

      :desc "Format buffer/region"        "F"          #'+format/region-or-buffer

      :desc "downcase"                    "J"          #'evil-downcase
      :desc "UpperCase"                   "K"          #'evil-upcase
      :desc "Decr"                        "j"          #'+jg-text-dec-num
      :desc "Incr"                        "k"          #'+jg-text-inc-num
      :desc "Shift Left"                  "h"          #'+jg-text-shift-left
      :desc "Remove Indentation"          "H"          #'+jg-text-remove-indentation
      :desc "Shift Right"                 "l"          #'+jg-text-shift-right
      :desc "Title Case"                  "t"          #'+jg-text-title-case-op

      :desc "Quick Change" "\"" #'evil-surround-change
      :desc "Quick Delete" "'"  #'evil-surround-delete
)

(map! :map jgb-change--text-map ;; Words
      :prefix "w"
      :desc "inflection" "i"    #'evil-operator-string-inflection
      :desc "Rotate"     "r"    #'rotate-text
       )

(map! :map jgb-change--text-map ;; Encoding
      :prefix "e"
      :desc "Rot13"                       "r"  #'evil-rot13
      :desc "Encode url"                  "u"  #'+evil:url-encode
      :desc "Decode url"                  "U"  #'+evil:url-decode
      :desc "ENCRYPT"                     "e"  #'+jg-text-encrypt-region
      :desc "DECRYPT"                     "E"  #'+jg-text-decrypt-region
      :desc "Accent"                      "a"  #'accent-menu
      )

(map! :map jgb-change--text-map ;; Lines
      :prefix "i"
      :desc "Wrap Line"                  "w"   #'evil-fill
      :desc "Fill"                       "W"   #'evil-fill-and-move
      :desc "Combine lines"              "c"   #'evil-join-whitespace
      :desc "Justify"                    "j"   #'justify-current-line
      )

(map! :map jgb-change--text-map ;; Text
      :prefix "o"
      :desc "Cycle Spacing"               "."  #'cycle-spacing
      :desc "Exchange"                    "x"  #'evil-exchange
      :desc "Split on distance"           "s"  #'+jg-text-split-on-leading-char
      :desc "Title Case"                  "t"  #'+jg-text-title-case-op
      )

(map! :map jgb-change--text-map ;; Surrounding
      :prefix "S"
      "d" #'evil-surround-delete
      "c" #'evil-surround-change
      "l" #'+jg-surround-list
      )

;;; change-map.el ends here
