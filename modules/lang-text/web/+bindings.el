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
      :desc "Render Buffer"      "r" #'shr-render-buffer
)

(map! :map web-mode-map ;; attributes
      :localleader
      :prefix ("a" . "attribute")
      :n "b" #'web-mode-attribute-beginning
      :n "e" #'web-mode-attribute-end
      :n "i" #'web-mode-attribute-insert
      :n "n" #'web-mode-attribute-next
      :n "s" #'web-mode-attribute-select
      :n "k" #'web-mode-attribute-kill
      :n "p" #'web-mode-attribute-previous
      :n "t" #'web-mode-attribute-transpose
)

(map! :map web-mode-map ;; block
      :localleader
      :prefix ("b" . "block")
      :n "b" #'web-mode-block-beginning
      :n "c" #'web-mode-block-close
      :n "e" #'web-mode-block-end
      :n "k" #'web-mode-block-kill
      :n "n" #'web-mode-block-next
      :n "p" #'web-mode-block-previous
      :n "s" #'web-mode-block-select
)

(map! :map web-mode-map ;; dom
      :localleader
      :prefix ("d" . "dom")
      :n "a" #'web-mode-dom-apostrophes-replace
      :n "d" #'web-mode-dom-errors-show
      :n "e" #'web-mode-dom-entities-encode
      :n "n" #'web-mode-dom-normalize
      :n "q" #'web-mode-dom-quotes-replace
      :n "t" #'web-mode-dom-traverse
      :n "x" #'web-mode-dom-xpath
      )

(map! :map web-mode-map ;; elements
      :localleader
      :prefix ("e" . "element")
      :n "/" #'web-mode-element-close
      :n "a" #'web-mode-element-content-select
      :n "b" #'web-mode-element-beginning
      :n "c" #'web-mode-element-clone
      :n "d" #'web-mode-element-child
      :n "e" #'web-mode-element-end
      :n "f" #'web-mode-element-children-fold-or-unfold
      :n "i" #'web-mode-element-insert
      :n "k" #'web-mode-element-kill
      :n "m" #'web-mode-element-mute-blanks
      :n "n" #'web-mode-element-next
      :n "p" #'web-mode-element-previous
      :n "r" #'web-mode-element-rename
      :n "s" #'web-mode-element-select
      :n "t" #'web-mode-element-transpose
      :n "u" #'web-mode-element-parent
      :n "v" #'web-mode-element-vanish
      :n "w" #'web-mode-element-wrap
      )

(map! :map web-mode-map ;; tags
      :localleader
      :prefix ("t" . "tag")
      :n "a" #'web-mode-tag-attributes-sort
      :n "b" #'web-mode-tag-beginning
      :n "e" #'web-mode-tag-end
      :n "m" #'web-mode-tag-match
      :n "n" #'web-mode-tag-next
      :n "p" #'web-mode-tag-previous
      :n "s" #'web-mode-tag-select
      )

(map! :map (css-mode-map less-css-mode-map scss-mode-map sass-mode-map)
      :n "|"  #'general-insert-call
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
