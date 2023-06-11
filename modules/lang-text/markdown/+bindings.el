;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-helm-map
      :desc "Post Ivy" "p" #'+jg-markdown-post-ivy

      )

(map! :map markdown-mode-map
        :localleader
        "'" #'markdown-edit-code-block
        "o" #'markdown-open
        "p" #'markdown-preview
        "e" #'markdown-export
        (:when (modulep! +grip)
         "p" #'grip-mode)

        )

(map! :map markdown-mode-map
      :prefix ("i" . "insert")
      :desc "Table Of Content"  "T" #'markdown-toc-generate-toc
      :desc "Image"             "i" #'markdown-insert-image
      :desc "Link"              "l" #'markdown-insert-link
      :desc "<hr>"              "-" #'markdown-insert-hr
      :desc "Heading 1"         "1" #'markdown-insert-header-atx-1
      :desc "Heading 2"         "2" #'markdown-insert-header-atx-2
      :desc "Heading 3"         "3" #'markdown-insert-header-atx-3
      :desc "Heading 4"         "4" #'markdown-insert-header-atx-4
      :desc "Heading 5"         "5" #'markdown-insert-header-atx-5
      :desc "Heading 6"         "6" #'markdown-insert-header-atx-6
      :desc "Code block"        "C" #'markdown-insert-gfm-code-block
      :desc "Pre region"        "P" #'markdown-pre-region
      :desc "Blockquote region" "Q" #'markdown-blockquote-region
      :desc "Checkbox"          "[" #'markdown-insert-gfm-checkbox
      :desc "Bold"              "b" #'markdown-insert-bold
      :desc "Inline code"       "c" #'markdown-insert-code
      :desc "Italic"            "e" #'markdown-insert-italic
      :desc "Footnote"          "f" #'markdown-insert-footnote
      :desc "Header dwim"       "h" #'markdown-insert-header-dwim
      :desc "Italic"            "i" #'markdown-insert-italic
      :desc "Kbd"               "k" #'markdown-insert-kbd
      :desc "Pre"               "p" #'markdown-insert-pre
      :desc "New blockquote"    "q" #'markdown-insert-blockquote
      :desc "Strike through"    "s" #'markdown-insert-strike-through
      :desc "Table"             "t" #'markdown-insert-table
      :desc "Wiki link"         "w" #'markdown-insert-wiki-link
)

(map! :map markdown-mode-map
      :prefix ("t" . "toggle")
      :desc "Inline LaTeX"      "e" #'markdown-toggle-math
      :desc "Code highlights"   "f" #'markdown-toggle-fontify-code-blocks-natively
      :desc "Inline images"     "i" #'markdown-toggle-inline-images
      :desc "URL hiding"        "l" #'markdown-toggle-url-hiding
      :desc "Markup hiding"     "m" #'markdown-toggle-markup-hiding
      :desc "Wiki links"        "w" #'markdown-toggle-wiki-links
      :desc "GFM checkbox"      "x" #'markdown-toggle-gfm-checkbox
)

(map! :map evil-markdown-mode-map
        :n "TAB" #'markdown-cycle
        :n [backtab] #'markdown-shifttab
        :i "M-*" #'markdown-insert-list-item
        :i "M-b" #'markdown-insert-bold
        :i "M-i" #'markdown-insert-italic
        :i "M-`" #'+markdown/insert-del
        :i "M--" #'markdown-insert-hr
        :n "M-r" #'browse-url-of-file
        :m "]h"  #'markdown-next-visible-heading
        :m "[h"  #'markdown-previous-visible-heading
        :m "[p"  #'markdown-promote
        :m "]p"  #'markdown-demote
        :m "[l"  #'markdown-previous-link
        :m "]l"  #'markdown-next-link
        )
