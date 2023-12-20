;;; +bindings.el -*- lexical-binding: t; -*-


(defvar-keymap jg-binding-markdown-map)

(map! :map jg-binding-helm-map
      :desc "Post Ivy" "p" #'+jg-markdown-post-ivy

      )

(map! :map jg-binding-markdown-map
        :localleader
        "'" #'markdown-edit-code-block
        "o" #'markdown-open
        "p" #'markdown-preview
        "e" #'markdown-export
        "r" #'markdown-toc-refresh-toc
        (:when (modulep! +grip)
         "p" #'grip-mode)

        )

(map! :map jg-binding-markdown-map
      :localleader
      :prefix ("i" . "insert")
      :desc "Table Of Content"   :n "T" #'markdown-toc-generate-toc
      :desc "Image"              :n "i" #'markdown-insert-image
      :desc "Link"               :n "l" #'markdown-insert-link
      :desc "<hr>"               :n "-" #'markdown-insert-hr
      :desc "Heading 1"          :n "1" #'markdown-insert-header-atx-1
      :desc "Heading 2"          :n "2" #'markdown-insert-header-atx-2
      :desc "Heading 3"          :n "3" #'markdown-insert-header-atx-3
      :desc "Heading 4"          :n "4" #'markdown-insert-header-atx-4
      :desc "Heading 5"          :n "5" #'markdown-insert-header-atx-5
      :desc "Heading 6"          :n "6" #'markdown-insert-header-atx-6
      :desc "Code block"         :n "C" #'markdown-insert-gfm-code-block
      :desc "Pre region"         :n "P" #'markdown-pre-region
      :desc "Blockquote region"  :n "Q" #'markdown-blockquote-region
      :desc "Checkbox"           :n "[" #'markdown-insert-gfm-checkbox
      :desc "Bold"               :n "b" #'markdown-insert-bold
      :desc "Inline code"        :n "c" #'markdown-insert-code
      :desc "Italic"             :n "e" #'markdown-insert-italic
      :desc "Footnote"           :n "f" #'markdown-insert-footnote
      :desc "Header dwim"        :n "h" #'markdown-insert-header-dwim
      :desc "Italic"             :n "i" #'markdown-insert-italic
      :desc "Kbd"                :n "k" #'markdown-insert-kbd
      :desc "Pre"                :n "p" #'markdown-insert-pre
      :desc "New blockquote"     :n "q" #'markdown-insert-blockquote
      :desc "Strike through"     :n "s" #'markdown-insert-strike-through
      :desc "Table"              :n "t" #'markdown-insert-table
      :desc "Wiki link"          :n "w" #'markdown-insert-wiki-link
)

(map! :map jg-binding-markdown-map
      :localleader
      :prefix ("t" . "toggle")
      :desc "Inline LaTeX"     :n "e" #'markdown-toggle-math
      :desc "Code highlights"  :n "f" #'markdown-toggle-fontify-code-blocks-natively
      :desc "Inline images"    :n "i" #'markdown-toggle-inline-images
      :desc "URL hiding"       :n "l" #'markdown-toggle-url-hiding
      :desc "Markup hiding"    :n "m" #'markdown-toggle-markup-hiding
      :desc "Wiki links"       :n "w" #'markdown-toggle-wiki-links
      :desc "GFM checkbox"     :n "x" #'markdown-toggle-gfm-checkbox
)

(map! :map jg-binding-markdown-map
        :n "TAB" #'markdown-cycle
        :n [backtab] #'markdown-shifttab
        ;; :i "M-*" #'markdown-insert-list-item
        ;; :i "M-b" #'markdown-insert-bold
        ;; :i "M-i" #'markdown-insert-italic
        ;; :i "M-`" #'+markdown/insert-del
        ;; :i "M--" #'markdown-insert-hr
        ;; :n "M-r" #'browse-url-of-file
        :m "]h"  #'markdown-next-visible-heading
        :m "[h"  #'markdown-previous-visible-heading
        :m "[p"  #'markdown-promote
        :m "]p"  #'markdown-demote
        :m "[l"  #'markdown-previous-link
        :m "]l"  #'markdown-next-link
        )

(after! (markdown-mode evil-markdown)
  (setq markdown-mode-map jg-binding-markdown-map
        evil-markdown-mode-map nil)
  )
