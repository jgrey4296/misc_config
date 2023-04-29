;;; lang-text/web/+bindings.el -*- lexical-binding: t; -*-

(map! :map web-mode-map
      ;; :g  "M-/" #'web-mode-comment-or-uncomment
      ;; :i  "SPC" #'self-insert-command
      :nv "]a"  #'web-mode-attribute-next
      :nv "[a"  #'web-mode-attribute-previous
      :nv "]t"  #'web-mode-tag-next
      :nv "[t"  #'web-mode-tag-previous
      :nv "]T"  #'web-mode-element-child
      :nv "[T"  #'web-mode-element-parent

      :m  "cex"  #'+web:encode-html-entities
      :m  "cex"  #'+web:decode-html-entities

      :localleader
      :desc "Rehighlight buffer" "h" #'web-mode-reload
      :desc "Indent buffer"      "i" #'web-mode-buffer-indent
)

(map! :map web-mode-map ;; attributes
      :prefix ("a" . "attribute")
      "b" #'web-mode-attribute-beginning
      "e" #'web-mode-attribute-end
      "i" #'web-mode-attribute-insert
      "n" #'web-mode-attribute-next
      "s" #'web-mode-attribute-select
      "k" #'web-mode-attribute-kill
      "p" #'web-mode-attribute-previous
      "t" #'web-mode-attribute-transpose
)

(map! :map web-mode-map ;; block
      :prefix ("b" . "block")
      "b" #'web-mode-block-beginning
      "c" #'web-mode-block-close
      "e" #'web-mode-block-end
      "k" #'web-mode-block-kill
      "n" #'web-mode-block-next
      "p" #'web-mode-block-previous
      "s" #'web-mode-block-select
)

(map! :map web-mode-map ;; dom
      :prefix ("d" . "dom")
      "a" #'web-mode-dom-apostrophes-replace
      "d" #'web-mode-dom-errors-show
      "e" #'web-mode-dom-entities-encode
      "n" #'web-mode-dom-normalize
      "q" #'web-mode-dom-quotes-replace
      "t" #'web-mode-dom-traverse
      "x" #'web-mode-dom-xpath
      )

(map! :map web-mode-map ;; elements
      :prefix ("e" . "element")
      "/" #'web-mode-element-close
      "a" #'web-mode-element-content-select
      "b" #'web-mode-element-beginning
      "c" #'web-mode-element-clone
      "d" #'web-mode-element-child
      "e" #'web-mode-element-end
      "f" #'web-mode-element-children-fold-or-unfold
      "i" #'web-mode-element-insert
      "k" #'web-mode-element-kill
      "m" #'web-mode-element-mute-blanks
      "n" #'web-mode-element-next
      "p" #'web-mode-element-previous
      "r" #'web-mode-element-rename
      "s" #'web-mode-element-select
      "t" #'web-mode-element-transpose
      "u" #'web-mode-element-parent
      "v" #'web-mode-element-vanish
      "w" #'web-mode-element-wrap)

(map! :map web-mode-map ;; tags
      :prefix ("t" . "tag")
      "a" #'web-mode-tag-attributes-sort
      "b" #'web-mode-tag-beginning
      "e" #'web-mode-tag-end
      "m" #'web-mode-tag-match
      "n" #'web-mode-tag-next
      "p" #'web-mode-tag-previous
      "s" #'web-mode-tag-select)

(map! :map (css-mode-map less-css-mode-map scss-mode-map sass-mode-map)
      :n "|" #'+css/toggle-inline-or-block
      :n "s j" #'counsel-css

      :localleader
      (:prefix ("b" . "build")
       :desc "Compile Less" :n "l" #'less-css-compile
      )
)

(map! :map +web-pelican-mode-map
      :i "{" (cmd! (insert "{%  %}") (backward-char 3))
      :i "}" (cmd! (insert "{{   }}") (backward-char 4))

      )
