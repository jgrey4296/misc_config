;;; +evil-text-obj-bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-inner-text-objects-map
      :desc "\""                   "\""  #'evil-inner-double-quote
      :desc "'"                    "'"   #'evil-inner-single-quote
      :desc "`'"                   "`"   #'evil-inner-back-quote
      :desc "("                    "("   #'evil-inner-paren
      :desc "<"                    "<"   #'evil-inner-angle
      :desc "["                    "["   #'evil-inner-bracket
      :desc "{"                    "{"   #'evil-inner-curly

      :desc "arg"                  "a"   #'evil-inner-arg
      :desc "paren"                "b"   #'evil-inner-paren
      :desc "block"                "B"   #'evil-textobj-anyblock-inner-block
      :desc "comment"              "c"   #'evilnc-inner-comment
      :desc "defun"                "f"   #'+evil:defun-txtobj
      :desc "whole-buffer"         "g"   #'+evil:whole-buffer-txtobj
      :desc "Same Indent Block"    "i"   #'evil-indent-plus-i-indent
      :desc "+Same Indent Block+"  "j"   #'evil-indent-plus-i-indent-up-down
      :desc "+Same Indent Block"   "k"   #'evil-indent-plus-i-indent-up
      :desc "Paragraph"            "p"   #'evil-inner-paragraph
      :desc "Any-Quote"            "q"   #'+evil:inner-any-quote
      :desc "Symbol"               "s"   #'evil-inner-symbol
      :desc "Sentence"             "S"   #'evil-inner-sentence
      :desc "XML Tag"              "t"   #'evil-inner-tag
      :desc "URL"                  "u"   #'+evil:inner-url-txtobj
      :desc "WORD"                 "W"   #'evil-inner-WORD
      :desc "word"                 "w"   #'evil-inner-word
      :desc "XML Attr"             "x"   #'evil-inner-xml-attr
      )
(map! :map jg-binding-outer-text-objects-map
      :desc  "\""                   "\"" #'evil-a-double-quote
      :desc  "'"                    "'"  #'evil-a-single-quote
      :desc  "`'"                   "`"  #'evil-a-back-quote
      :desc  "("                    "("  #'evil-a-paren
      :desc  "<"                    "<"  #'evil-an-angle
      :desc  "["                    "["  #'evil-a-bracket
      :desc  "{"                    "{"  #'evil-a-curly

      :desc  "arg"                  "a"  #'evil-outer-arg
      :desc  "paren"                "b"  #'evil-a-paren
      :desc  "lock"                 "B"  #'evil-textobj-anyblock-a-block
      :desc  "comment"              "c"  #'evilnc-outer-commenter
      :desc  "defun"                "f"  #'+evil:defun-txtobj
      :desc  "whole-buffer"         "g"  #'+evil:whole-buffer-txtobj
      :desc  "Same Indent Block"    "i"  #'evil-indent-plus-a-indent
      :desc  "+Same Indent Block+"  "j"  #'evil-indent-plus-a-indent-up-down
      :desc  "+Same Indent Block"   "k"  #'evil-indent-plus-a-indent-up
      :desc  "Paragraph"            "p"  #'evil-a-paragraph
      :desc  "Any-Quote"            "q"  #'+evil:outer-any-quote
      :desc  "Symbol"               "s"  #'evil-a-symbol
      :desc  "Sentence"             "S"  #'evil-a-sentence
      :desc  "XML Tag"              "t"  #'evil-a-tag
      :desc  "URL"                  "u"  #'+evil:outer-url-txtobj
      :desc  "WORD"                 "W"  #'evil-a-WORD
      :desc  "word"                 "w"  #'evil-a-word
      :desc  "XML attr"             "x"  #'evil-outer-xml-attr
      )
