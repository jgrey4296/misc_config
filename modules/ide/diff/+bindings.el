;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map diff-hl-show-hunk-map
      :after diff-hl-show-hunk
      :n "p" #'diff-hl-show-hunk-previous
      :n "n" #'diff-hl-show-hunk-next
      :n "c" #'diff-hl-show-hunk-copy-original-text
      :n "r" #'diff-hl-show-hunk-revert-hunk
      :n "[" #'diff-hl-show-hunk-previous
      :n "]" #'diff-hl-show-hunk-next
      :n "{" #'diff-hl-show-hunk-previous
      :n "}" #'diff-hl-show-hunk-next
      :n "S" #'diff-hl-show-hunk-stage-hunk
      )
